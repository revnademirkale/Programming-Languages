%{
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

// Interface with Flex
int yylex();
void yyerror(const char *s);
extern int current_line;

// ================== CUSTOM DATA STRUCTURES ==================

// Symbol Table Entry (Linked List for function definitions)
typedef struct Symbol {
    char key;           // Function name (single char: f, g...)
    char *arg_names;    // Parameter names as a string
    int arg_count;      // Number of parameters (arity)
    int decl_row;       // Line number where it is defined
    char *formula;      // Function body expression as a string
    struct Symbol *next;
} Symbol;

// Error Logging Structure (Linked List)
typedef struct LogNode {
    int row_idx;        // Line number of the error
    char *msg_text;     // Error message content
    struct LogNode *next;
} LogNode;

// Calculation Command Structure (Queued operations)
typedef struct CalcCmd {
    char *raw_txt;      // Original input string for display
    char *math_expr;    // Parsed expression to be evaluated
    struct CalcCmd *next;
} CalcCmd;

// Mathematical Monomial (Term in a polynomial)
// Represents: (coeff_n / coeff_d) * a^pows[0] * b^pows[1] ...
typedef struct Monomial {
    long long coeff_n;  // Numerator of the coefficient
    long long coeff_d;  // Denominator of the coefficient
    int pows[26];       // Exponents for variables 'a' through 'z'
    struct Monomial *next;
} Monomial;

// Global List Pointers
Symbol *g_sym_root = NULL;
LogNode *g_log_root = NULL;
CalcCmd *g_calc_root = NULL;
int g_syntax_err = 0; // Flag to indicate if semantic errors occurred

// ================== FUNCTION PROTOTYPES ==================
// Symbol Table & Error Management
void reg_symbol(char key, char *args, int count, char *form);
void push_error(int r, const char *fmt, char arg);
void push_calc(char *orig, char *expr);
Symbol* fetch_sym(char key);

// String Utilities
char* str_clean(const char *in);
char* str_sort_uniq(const char *in);
int has_char(const char *s, char c);

// Math Processing
Monomial* parse_math_str(const char *s);
void print_math(Monomial *head);
void free_math(Monomial *head);
long long calc_gcd(long long a, long long b);

// ================== MATHEMATICAL ALGORITHMS ==================

// Calculate Greatest Common Divisor (Iterative Algorithm)
long long calc_gcd(long long a, long long b) {
    long long temp;
    a = llabs(a);
    b = llabs(b);
    while (b != 0) {
        temp = b;
        b = a % b;
        a = temp;
    }
    return a;
}

// Reduce fraction to simplest form
void reduce(long long *n, long long *d) {
    if (*d == 0) return;
    long long common = calc_gcd(*n, *d);
    *n /= common;
    *d /= common;
    // Ensure denominator is positive
    if (*d < 0) { *n = -(*n); *d = -(*d); }
}

// Create a new monomial term
Monomial* make_mono(long long n, long long d) {
    Monomial *m = (Monomial*)calloc(1, sizeof(Monomial));
    m->coeff_n = n;
    m->coeff_d = d;
    reduce(&m->coeff_n, &m->coeff_d);
    return m;
}

// Deep copy of a monomial
Monomial* clone_mono(Monomial *src) {
    if (!src) return NULL;
    Monomial *dst = make_mono(src->coeff_n, src->coeff_d);
    memcpy(dst->pows, src->pows, sizeof(int)*26);
    return dst;
}

// Compare two monomials for canonical ordering
// Returns -1 if 'a' > 'b', 1 if 'b' > 'a', 0 if equal powers
int compare_mono(Monomial *a, Monomial *b) {
    for (int i = 0; i < 26; i++) {
        if (a->pows[i] > b->pows[i]) return -1; // Higher power comes first
        if (a->pows[i] < b->pows[i]) return 1;
    }
    return 0; // Exponents are identical
}

// Insert a monomial into the list while maintaining sorted order
// Handles combining like terms (addition)
void add_sorted(Monomial **head, Monomial *new_m) {
    if (new_m->coeff_n == 0) { free(new_m); return; }

    Monomial *curr = *head;
    Monomial *prev = NULL;

    while (curr) {
        int cmp = compare_mono(new_m, curr);
        
        if (cmp == 0) { // Same exponents: Add coefficients
            long long nn = curr->coeff_n * new_m->coeff_d + new_m->coeff_n * curr->coeff_d;
            long long nd = curr->coeff_d * new_m->coeff_d;
            curr->coeff_n = nn;
            curr->coeff_d = nd;
            reduce(&curr->coeff_n, &curr->coeff_d);
            
            if (curr->coeff_n == 0) { // If result is zero, remove the term
                if (prev) prev->next = curr->next;
                else *head = curr->next;
                free(curr);
            }
            free(new_m);
            return;
        }
        
        if (cmp < 0) { // new_m should be placed before curr
            new_m->next = curr;
            if (prev) prev->next = new_m;
            else *head = new_m;
            return;
        }
        
        prev = curr;
        curr = curr->next;
    }
    // Insert at the end
    if (prev) prev->next = new_m;
    else *head = new_m;
}

// Multiply two polynomials
Monomial* poly_mult(Monomial *p1, Monomial *p2) {
    Monomial *res = NULL;
    for (Monomial *t1 = p1; t1; t1 = t1->next) {
        for (Monomial *t2 = p2; t2; t2 = t2->next) {
            Monomial *prod = make_mono(t1->coeff_n * t2->coeff_n, t1->coeff_d * t2->coeff_d);
            for (int k = 0; k < 26; k++) prod->pows[k] = t1->pows[k] + t2->pows[k];
            add_sorted(&res, prod);
        }
    }
    return res ? res : make_mono(0, 1);
}

// Substitute a variable with a polynomial expression
Monomial* poly_subst(Monomial *poly, int var_idx, Monomial *val) {
    Monomial *res = NULL;
    Monomial *curr = poly;
    
    while (curr) {
        Monomial *base = clone_mono(curr);
        int power = base->pows[var_idx];
        base->pows[var_idx] = 0; // Remove the variable being replaced
        
        if (power == 0) {
            add_sorted(&res, base);
        } else {
            // Calculate (val)^power
            Monomial *pow_res = make_mono(1, 1);
            for (int i = 0; i < power; i++) {
                Monomial *tmp = poly_mult(pow_res, val);
                free_math(pow_res);
                pow_res = tmp;
            }
            // Multiply base coefficient with the result
            Monomial *term_res = poly_mult(base, pow_res);
            
            for (Monomial *t = term_res; t; t = t->next) {
                add_sorted(&res, clone_mono(t));
            }
            free_math(pow_res);
            free_math(term_res);
            free(base);
        }
        curr = curr->next;
    }
    return res;
}

// Compute Derivative
Monomial* poly_deriv(Monomial *poly, int var_idx, int deg) {
    Monomial *res = NULL;
    for (Monomial *curr = poly; curr; curr = curr->next) {
        Monomial *t = clone_mono(curr);
        int p = t->pows[var_idx];
        
        if (p < deg) {
            free(t); // Term becomes zero
        } else {
            long long mult = 1;
            for (int i = 0; i < deg; i++) mult *= (p - i);
            t->coeff_n *= mult;
            reduce(&t->coeff_n, &t->coeff_d);
            t->pows[var_idx] -= deg;
            add_sorted(&res, t);
        }
    }
    return res ? res : make_mono(0, 1);
}

// Compute Integral
Monomial* poly_integ(Monomial *poly, int var_idx, int deg) {
    Monomial *res = NULL;
    for (Monomial *curr = poly; curr; curr = curr->next) {
        Monomial *t = clone_mono(curr);
        int p = t->pows[var_idx];
        
        for (int i = 0; i < deg; i++) {
            p++;
            t->coeff_d *= p; // Increase denominator
            t->pows[var_idx] = p;
            reduce(&t->coeff_n, &t->coeff_d);
        }
        add_sorted(&res, t);
    }
    return res ? res : make_mono(0, 1);
}

// ================== RECURSIVE DESCENT PARSER LOGIC ==================
// Parses the string representation of math into Monomial linked lists
/*
   parse_factor_level:
   Handles:
   - numbers
   - variables
   - parentheses
   - function calls
   - D(...) / I(...)
*/
Monomial* parse_add_level(const char **s);

Monomial* parse_factor_level(const char **s) {
    while (**s && isspace(**s)) (*s)++;
    
    // Handle Parentheses
    if (**s == '(') {
        (*s)++;
        Monomial *ret = parse_add_level(s);
        while (**s && isspace(**s)) (*s)++;
        if (**s == ')') (*s)++;
        return ret;
    }
    // Handle Numbers
    if (isdigit(**s)) {
        long long val = 0;
        while (isdigit(**s)) { val = val * 10 + (*(*s)++ - '0'); }
        return make_mono(val, 1);
    }
    // Handle Identifiers (Variables or Functions)
    if (isalpha(**s)) {
        char name = *(*s)++;
        while (**s && isspace(**s)) (*s)++;
        
        // Just a variable (x, y, etc.)
        if (**s != '(') { 
            Monomial *m = make_mono(1, 1);
            if (islower(name)) m->pows[name - 'a'] = 1;
            return m;
        }
        
        (*s)++; // Skip '('
        
        // Check for Derivation or Integration Commands
        if (name == 'D' || name == 'I') {
            int is_deriv = (name == 'D');
            while(isspace(**s)) (*s)++; char fname = *(*s)++;
            while(**s != ',') (*s)++; (*s)++; 
            while(isspace(**s)) (*s)++; char vname = *(*s)++;
            while(**s != ',') (*s)++; (*s)++;
            long long deg = 0;
            while(isspace(**s)) (*s)++;
            while(isdigit(**s)) deg = deg * 10 + (*(*s)++ - '0');
            while(**s != ')') (*s)++; (*s)++;
            
            Symbol *sym = fetch_sym(fname);
            if (!sym) return make_mono(0, 1);
            
            Monomial *body = parse_math_str(sym->formula);
            Monomial *ret = is_deriv ? poly_deriv(body, vname-'a', deg) 
                                     : poly_integ(body, vname-'a', deg);
            free_math(body);
            return ret;
        } 
        else { // Standard Function Call f(x,y)
            Symbol *sym = fetch_sym(name);
            Monomial *args[10]; int ac = 0;
            
            while (1) {
                while(isspace(**s)) (*s)++;
                if (**s == ')') break;
                args[ac++] = parse_add_level(s);
                while(isspace(**s)) (*s)++;
                if (**s == ',') (*s)++; else break;
            }
            if (**s == ')') (*s)++;

            if (!sym) { // If undefined, treat as variable * arguments
                Monomial *base = make_mono(1, 1);
                if (islower(name)) base->pows[name-'a'] = 1;
                if (ac > 0) {
                    Monomial *res = poly_mult(base, args[0]);
                    free_math(base); free_math(args[0]);
                    return res;
                }
                return base;
            }
            // If defined, perform substitution
            Monomial *curr = parse_math_str(sym->formula);
            for (int i=0; i<sym->arg_count && i<ac; i++) {
                Monomial *nxt = poly_subst(curr, sym->arg_names[i]-'a', args[i]);
                free_math(curr);
                curr = nxt;
            }
            for(int i=0; i<ac; i++) free_math(args[i]);
            return curr;
        }
    }
    return make_mono(1, 1);
}

// Parses Terms with Exponents (^)
Monomial* parse_term_level(const char **s) {
    Monomial *lhs = parse_factor_level(s);
    while (**s && isspace(**s)) (*s)++;
    
    if (**s == '^') {
        (*s)++;
        while (**s && isspace(**s)) (*s)++;
        long long exp = 0;
        while (isdigit(**s)) exp = exp * 10 + (*(*s)++ - '0');
        
        for (int i=0; i<26; i++) lhs->pows[i] *= exp;
        long long nn=1, nd=1;
        for (int i=0; i<exp; i++) { nn *= lhs->coeff_n; nd *= lhs->coeff_d; }
        lhs->coeff_n = nn; lhs->coeff_d = nd;
        reduce(&lhs->coeff_n, &lhs->coeff_d);
    }
    return lhs;
}

// Parses Multiplication (*)
Monomial* parse_mult_level(const char **s) {
    Monomial *res = NULL;
    while (1) {
        while (**s && isspace(**s)) (*s)++;
        if (!**s || **s=='+' || **s=='-' || **s==')' || **s==',' || **s==';') break;
        
        Monomial *rhs = parse_term_level(s);
        if (!res) res = rhs;
        else {
            Monomial *prod = poly_mult(res, rhs);
            free_math(res); free_math(rhs);
            res = prod;
        }
        while (**s && isspace(**s)) (*s)++;
        if (**s == '*') (*s)++;
    }
    return res ? res : make_mono(1, 1);
}

// Parses Addition and Subtraction (+/-)
Monomial* parse_add_level(const char **s) {
    Monomial *res = NULL;
    int neg = 0;
    while (**s && isspace(**s)) (*s)++;
    if (**s == '-') { neg=1; (*s)++; } else if (**s == '+') (*s)++;
    
    while (1) {
        Monomial *term = parse_mult_level(s);
        if (neg) term->coeff_n = -term->coeff_n;
        
        for (Monomial *t = term; t; t = t->next) {
            add_sorted(&res, clone_mono(t));
        }
        free_math(term);
        
        while (**s && isspace(**s)) (*s)++;
        if (**s == '+') { (*s)++; neg=0; }
        else if (**s == '-') { (*s)++; neg=1; }
        else break;
    }
    return res;
}

// Entry point for math string parsing
Monomial* parse_math_str(const char *expr) {
    const char *ptr = expr;
    return parse_add_level(&ptr);
}

void free_math(Monomial *head) {
    while (head) { Monomial *tmp = head; head = head->next; free(tmp); }
}

// ================== MANAGEMENT & OUTPUT ==================

// Register a new function in the symbol table
void reg_symbol(char key, char *args, int count, char *form) {
    Symbol *s = (Symbol*)malloc(sizeof(Symbol));
    s->key = key;
    s->arg_names = args ? strdup(args) : NULL;
    s->arg_count = count;
    s->decl_row = current_line;
    s->formula = form ? strdup(form) : NULL;
    s->next = g_sym_root;
    g_sym_root = s;
}

Symbol* fetch_sym(char key) {
    for (Symbol *s = g_sym_root; s; s = s->next)
        if (s->key == key) return s;
    return NULL;
}

// Add an error to the log
void push_error(int r, const char *fmt, char arg) {
    char buf[256];
    if (arg) sprintf(buf, "%s (%c)", fmt, arg);
    else sprintf(buf, "%s", fmt);
    
    LogNode *e = (LogNode*)malloc(sizeof(LogNode));
    e->row_idx = r;
    e->msg_text = strdup(buf);
    e->next = g_log_root;
    g_log_root = e;
    g_syntax_err = 1;
}

// Queue a calculation command
void push_calc(char *orig, char *expr) {
    CalcCmd *c = (CalcCmd*)malloc(sizeof(CalcCmd));
    c->raw_txt = strdup(orig);
    c->math_expr = strdup(expr);
    c->next = g_calc_root;
    g_calc_root = c;
}

// Sort errors using Bubble Sort and print them
void flush_errors() {
    if (!g_log_root) return;
    
    // Convert linked list to array for easier sorting
    int count = 0;
    for (LogNode *e = g_log_root; e; e = e->next) count++;
    LogNode **arr = (LogNode**)malloc(count * sizeof(LogNode*));
    int i = 0;
    for (LogNode *e = g_log_root; e; e = e->next) arr[i++] = e;
    
    // Bubble Sort Algorithm
    for (int step = 0; step < count - 1; step++) {
        for (int j = 0; j < count - step - 1; j++) {
            int swap = 0;
            // Primary sort key: Line Number
            if (arr[j]->row_idx > arr[j+1]->row_idx) swap = 1;
            // Secondary sort key: Message Content
            else if (arr[j]->row_idx == arr[j+1]->row_idx) {
                if (strcmp(arr[j]->msg_text, arr[j+1]->msg_text) > 0) swap = 1;
            }
            
            if (swap) {
                LogNode *tmp = arr[j];
                arr[j] = arr[j+1];
                arr[j+1] = tmp;
            }
        }
    }
    
    // Print sorted errors
    for (int k = 0; k < count; k++) {
        printf("%d %s\n", arr[k]->row_idx, arr[k]->msg_text);
        free(arr[k]->msg_text); free(arr[k]);
    }
    free(arr);
}

// Print a polynomial in canonical form
void print_math(Monomial *head) {
    if (!head || (head->coeff_n == 0 && !head->next)) { printf("0"); return; }
    int first = 1;
    for (Monomial *curr = head; curr; curr = curr->next) {
        if (curr->coeff_n == 0) continue;
        if (!first && curr->coeff_n > 0) printf("+");
        
        int has_var = 0;
        for (int i=0; i<26; i++) if (curr->pows[i]) has_var = 1;
        
        long long an = llabs(curr->coeff_n);
        if (curr->coeff_d == 1) {
            if (an != 1 || !has_var) {
                if (curr->coeff_n < 0 && (first || curr->coeff_n != -1)) printf("-");
                else if (curr->coeff_n == -1 && !first) printf("-");
                if (an != 1 || !has_var) printf("%lld", an);
            } else if (curr->coeff_n == -1) printf("-");
        } else {
            if (curr->coeff_n < 0 && first) printf("-");
            printf("%lld/%lld", an, curr->coeff_d);
        }
        
        for (int i=0; i<26; i++) {
            if (curr->pows[i]) {
                printf("%c", 'a'+i);
                if (curr->pows[i] > 1) printf("^%d", curr->pows[i]);
            }
        }
        first = 0;
    }
}

// Execute all queued calculations
void run_calculator() {
    // Reverse the list to process in FIFO order
    CalcCmd *prev = NULL, *curr = g_calc_root, *next = NULL;
    while (curr) { next = curr->next; curr->next = prev; prev = curr; curr = next; }
    g_calc_root = prev;
    
    for (CalcCmd *c = g_calc_root; c; c = c->next) {
        printf("%s=", c->raw_txt);
        Monomial *res = parse_math_str(c->math_expr);
        print_math(res);
        printf("\n");
        free_math(res);
        free(c->raw_txt); free(c->math_expr); free(c);
    }
}

// Utility: Clean whitespace from string
char* str_clean(const char *s) {
    if (!s) return NULL;
    char *res = malloc(strlen(s)+1);
    int j=0;
    for(int i=0; s[i]; i++) if(!isspace(s[i])) res[j++] = s[i];
    res[j] = 0;
    return res;
}

// Utility: Sort characters in a string and remove duplicates
char* str_sort_uniq(const char *s) {
    if (!s) return NULL;
    char *dup = strdup(s);
    int len = strlen(dup);
    // Simple Bubble Sort for char array
    for(int i=0; i<len; i++)
        for(int j=0; j<len-1; j++)
            if(dup[j] > dup[j+1]) { char t=dup[j]; dup[j]=dup[j+1]; dup[j+1]=t; }
    // Remove duplicates
    int u = 0;
    for(int i=0; i<len; i++) {
        if (i==0 || dup[i] != dup[i-1]) dup[u++] = dup[i];
    }
    dup[u] = 0;
    return dup;
}

int has_char(const char *s, char c) {
    if(!s) return 0;
    while(*s) if(*s++ == c) return 1;
    return 0;
}

void yyerror(const char *s) { 
    /* Silent on syntax errors as per requirements */
}

%}

%union {
    char char_val;
    int int_val;
    char *str_val;
    struct { char *p; int n; } param_struct;
}

%token tCALCULATE tDERIVATION tINTEGRATION
%token tPLUS tMINUS tMUL tEXP
%token tLPR tRPR tASSIGN tCOMMA tSEMICOLON
%token <char_val> tIDENTIFIER
%token <int_val> tINTEGER

%type <param_struct> params
%type <str_val> expr term factor integ_cmd deriv_cmd calc_expr calc_term calc_factor calc_args

%start prog

%%

prog:
    defs calcs
    | defs
    | calcs
    | /* empty */
    ;

defs:
    def
    | def defs
    ;

def:
    tIDENTIFIER tLPR params tRPR tASSIGN expr tSEMICOLON
    {
        int line = current_line;
        if (fetch_sym($1)) push_error(line, "REDEFINED FUNCTION", $1);
        
        char *vars = str_sort_uniq($6);
        for(int i=0; vars && vars[i]; i++) {
            if (vars[i] >= 'a' && vars[i] <= 'z' && !has_char($3.p, vars[i]))
                push_error(line, "UNDEFINED FUNCTION PARAMETER", vars[i]);
        }
        if(vars) free(vars);
        
        reg_symbol($1, $3.p, $3.n, $6);
        if($3.p) free($3.p); if($6) free($6);
    }
    | tIDENTIFIER tLPR tRPR tASSIGN expr tSEMICOLON
    {
        int line = current_line;
        if (fetch_sym($1)) push_error(line, "REDEFINED FUNCTION", $1);
        
        char *vars = str_sort_uniq($5);
        for(int i=0; vars && vars[i]; i++) {
            if (vars[i] >= 'a' && vars[i] <= 'z')
                push_error(line, "UNDEFINED FUNCTION PARAMETER", vars[i]);
        }
        if(vars) free(vars);
        
        reg_symbol($1, NULL, 0, $5);
        if($5) free($5);
    }
    ;

params:
    tIDENTIFIER 
    { 
        $$.p = malloc(2); $$.p[0]=$1; $$.p[1]=0; 
        $$.n=1; 
    }
    | tIDENTIFIER tCOMMA params 
    { 
        $$.n = $3.n + 1;
        $$.p = malloc($$.n + 1);
        $$.p[0] = $1;
        strcpy($$.p+1, $3.p);
        free($3.p);
    }
    ;

/* Expression String Building */
expr:
    expr tPLUS term 
    { 
        $$ = malloc(strlen($1)+strlen($3)+2); sprintf($$, "%s+%s", $1, $3); 
        free($1); free($3); 
    }
    | expr tMINUS term 
    { 
        $$ = malloc(strlen($1)+strlen($3)+2); sprintf($$, "%s-%s", $1, $3); 
        free($1); free($3); 
    }
    | term { $$ = $1; }
    ;

term:
    term tMUL factor
    { 
        $$ = malloc(strlen($1)+strlen($3)+2); sprintf($$, "%s*%s", $1, $3); 
        free($1); free($3); 
    }
    | term factor 
    { 
        $$ = malloc(strlen($1)+strlen($2)+1); sprintf($$, "%s%s", $1, $2); 
        free($1); free($2); 
    }
    | factor { $$ = $1; }
    ;

factor:
    tINTEGER { $$ = malloc(20); sprintf($$, "%d", $1); }
    | tIDENTIFIER { $$ = malloc(2); $$[0]=$1; $$[1]=0; }
    | tLPR expr tRPR { $$ = malloc(strlen($2)+3); sprintf($$, "(%s)", $2); free($2); }
    | tIDENTIFIER tEXP tINTEGER { $$ = malloc(20); sprintf($$, "%c^%d", $1, $3); }
    | tINTEGER tEXP tINTEGER { $$ = malloc(30); sprintf($$, "%d^%d", $1, $3); }
    ;

calcs:
    calc_ln | calc_ln calcs ;

calc_ln:
    tCALCULATE calc_expr tSEMICOLON 
    { 
        char *clean = str_clean($2);
        push_calc(clean, $2);
        free(clean); free($2);
    }
    ;

calc_expr:
    calc_expr tPLUS calc_term
    { 
        $$ = malloc(strlen($1)+strlen($3)+2); sprintf($$, "%s+%s", $1, $3); 
        free($1); free($3); 
    }
    | calc_expr tMINUS calc_term
    { 
        $$ = malloc(strlen($1)+strlen($3)+2); sprintf($$, "%s-%s", $1, $3); 
        free($1); free($3); 
    }
    | calc_term { $$ = $1; }
    ;

calc_term:
    calc_term tMUL calc_factor
    { 
        $$ = malloc(strlen($1)+strlen($3)+2); sprintf($$, "%s*%s", $1, $3); 
        free($1); free($3); 
    }
    | calc_term calc_factor
    { 
        $$ = malloc(strlen($1)+strlen($2)+1); sprintf($$, "%s%s", $1, $2); 
        free($1); free($2); 
    }
    | calc_factor { $$ = $1; }
    ;

calc_factor:
    tINTEGER { $$ = malloc(20); sprintf($$, "%d", $1); }
    | tIDENTIFIER { $$ = malloc(2); $$[0]=$1; $$[1]=0; }
    | tLPR tRPR { $$ = strdup("()"); }
    | tLPR calc_args tRPR 
    { 
        $$ = malloc(strlen($2)+3); sprintf($$, "(%s)", $2); 
        if(strchr($2, ',')) push_error(current_line, "MISSING FUNCTION NAME", 0);
        free($2);
    }
    | tIDENTIFIER tEXP tINTEGER { $$ = malloc(20); sprintf($$, "%c^%d", $1, $3); }
    | tINTEGER tEXP tINTEGER { $$ = malloc(30); sprintf($$, "%d^%d", $1, $3); }
    | tIDENTIFIER tLPR calc_args tRPR 
    { 
        int c=1; for(int i=0; $3[i]; i++) if($3[i]==',') c++;
        Symbol *s = fetch_sym($1);
        if(!s) push_error(current_line, "UNDEFINED FUNCTION", $1);
        else if(s->arg_count != c) push_error(current_line, "ARITY CONTRADICTION", $1);
        $$ = malloc(strlen($3)+10); sprintf($$, "%c(%s)", $1, $3); free($3);
    }
    | tIDENTIFIER tLPR tRPR
    {
        Symbol *s = fetch_sym($1);
        if(!s) push_error(current_line, "UNDEFINED FUNCTION", $1);
        else if(s->arg_count != 0) push_error(current_line, "ARITY CONTRADICTION", $1);
        $$ = malloc(5); sprintf($$, "%c()", $1);
    }
    | integ_cmd { $$ = $1; }
    | deriv_cmd { $$ = $1; }
    ;

calc_args:
    calc_expr { $$ = $1; }
    | calc_args tCOMMA calc_expr 
    { 
        $$ = malloc(strlen($1)+strlen($3)+2); sprintf($$, "%s,%s", $1, $3); 
        free($1); free($3); 
    }
    ;

integ_cmd:
    tINTEGRATION tLPR tIDENTIFIER tCOMMA tIDENTIFIER tCOMMA tINTEGER tRPR
    {
        Symbol *s = fetch_sym($3);
        if(!s) push_error(current_line, "UNDEFINED FUNCTION FOR INTEGRATION", $3);
        else if(!has_char(s->arg_names, $5)) push_error(current_line, "UNDEFINED VARIABLE FOR INTEGRATION", $5);
        $$ = malloc(30); sprintf($$, "I(%c,%c,%d)", $3, $5, $7);
    }
    ;

deriv_cmd:
    tDERIVATION tLPR tIDENTIFIER tCOMMA tIDENTIFIER tCOMMA tINTEGER tRPR
    {
        Symbol *s = fetch_sym($3);
        if(!s) push_error(current_line, "UNDEFINED FUNCTION FOR DERIVATION", $3);
        else if(!has_char(s->arg_names, $5)) push_error(current_line, "UNDEFINED VARIABLE FOR DERIVATION", $5);
        $$ = malloc(30); sprintf($$, "D(%c,%c,%d)", $3, $5, $7);
    }
    ;

%%
/*
   Main:
   - run parser
   - if any semantic error exists → print errors
   - otherwise → run calculator
*/

int main() {
    if (yyparse()) return 1;
    if (g_syntax_err) flush_errors();
    else run_calculator();
    return 0;
}