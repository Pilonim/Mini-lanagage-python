import ply.yacc as yacc
from genereTreeGraphviz2 import printTreeGraph

reserved = {
    'print': 'PRINT',
    'printString': 'PRINTSTRING',
    'if': 'IF',
    'for': 'FOR',
    'while': 'WHILE',
    'else': 'ELSE',
    'voidFunction': 'VOIDFUNCTION',
    'function': 'FUNCTION'
}

tokens = [
    'NUMBER','MINUS',
    'PLUS','TIMES','DIVIDE',
    'LPAREN','RPAREN',
    'LACOL','RACOL','DOUBLEC',
    'AND', 'OR', 'SEMICOLON', 'NAME', 'EGAL', 'HIGHER', 'LOWER','COMA'
    ] + list(reserved.values())

precedence = (
    ('right', 'AND', 'OR'),
    ('nonassoc', 'LOWER', 'HIGHER', "EGAL"),
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIVIDE'),
)

# Tokens
t_PLUS      = r'\+'
t_MINUS     = r'-'
t_TIMES     = r'\*'
t_DIVIDE    = r'/'
t_LPAREN    = r'\('
t_RPAREN    = r'\)'
t_LACOL     = r'{'
t_RACOL     = r'}'
t_DOUBLEC   = r'"'
t_AND       = r'&'
t_OR        = r'\|'
t_SEMICOLON = r';'
t_COMA      = r','
t_EGAL      = r'='
t_LOWER     = r'<'
t_HIGHER    = r'>'

def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t


def t_NAME(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value, 'NAME')  # Check for reserved words
    return t

# Ignored characters
t_ignore = " \t"

def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")

def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

# Build the lexer
import ply.lex as lex
lex.lex()


def p_start(p):
    '''start : bloc'''
    print('Arbre de dérivation = ', p[1])
    printTreeGraph(p[1])
    print('CALC> ',end="")
    evalInst(p[1])

def p_bloc(p):
    '''bloc : bloc statement SEMICOLON
            | bloc expression SEMICOLON
            | statement SEMICOLON
            | expression SEMICOLON
            | statement
            | expression'''
    if len(p) == 4:
        p[0] = ('bloc', p[1], p[2])
    else:
        p[0] = ('bloc', p[1], 'empty')

def p_param(p):
    '''param : NAME COMA param
        | NAME'''
    if len(p) > 2:
        p[0] = ('param',p[1],p[3])
    else:
        p[0] = ('param',p[1],'empty')

def p_args(p):
    '''args : NAME COMA args
            | NUMBER COMA args
            | NAME
            | NUMBER'''
    if len(p) > 2:
        p[0] = ('args',p[1],p[3])
    else:
        p[0] = ('args',p[1],'empty')

def p_statement_expr(p):
    '''statement : PRINT LPAREN expression RPAREN
                | PRINT LPAREN statement RPAREN
                | PRINTSTRING LPAREN DOUBLEC NAME DOUBLEC RPAREN
                | IF LPAREN expression RPAREN LACOL bloc RACOL
                | IF LPAREN expression RPAREN LACOL bloc RACOL ELSE LACOL bloc RACOL
                | FOR LPAREN statement SEMICOLON expression SEMICOLON statement RPAREN LACOL bloc RACOL
                | WHILE LPAREN expression RPAREN LACOL bloc RACOL'''
    if p[1] == 'print': p[0] = ('print', p[3])
    if p[1] == 'if':
        if len(p) > 8:
            p[0] = ('if', p[3], p[6], p[10])
        else:
            p[0] = ('if', p[3], p[6])
    if p[1] == 'for': p[0] = ('for', p[3], p[5], p[7], p[10])
    if p[1] == 'while': p[0] = ('while', p[3], p[6])
    if p[1] == 'printString': p[0] = ('print', p[4])

def p_statement_functions(p):
    '''statement : VOIDFUNCTION NAME LPAREN RPAREN LACOL bloc RACOL
        | FUNCTION NAME LPAREN param RPAREN LACOL bloc RACOL'''
    if(p[1] == 'voidFunction'):
        p[0] = ('voidFunction', (p[2], p[6], 'empty'), 'empty')
    else:
        p[0] = ('function', (p[2], p[7], p[4]), 'empty')


def p_statement_call(p):
    'statement : NAME LPAREN RPAREN'
    p[0] = ('call', p[1], 'empty')

def p_statement_callParam(p):
    'statement : NAME LPAREN args RPAREN'
    p[0] = ('callParam', p[1], p[3])

def p_statement_variable(p):
    '''statement : NAME EGAL expression'''
    if len(p) > 2:
        p[0] = ('assign', p[1], p[3])

def p_expression_variable(p):
    'expression : NAME'
    p[0] = p[1]

def p_expression_binop_plus(p):
    '''expression : expression PLUS expression
        | NAME PLUS expression'''
    p[0] = ('+', p[1], p[3])

def p_statement_binop_plus_minus_2(p):
    '''statement : NAME PLUS PLUS
                | NAME MINUS MINUS'''
    p[0] = ('assign', p[1], (p[2], p[1], 1))


def p_statement_binop_operator_egal(p):
    '''statement : NAME PLUS EGAL expression
                    | NAME MINUS EGAL expression
                    | NAME TIMES EGAL expression
                    | NAME DIVIDE EGAL expression'''
    p[0] = ('assign', p[1], (p[2], p[1], p[4]))


def p_expression_binop_times(p):
    'expression : expression TIMES expression'
    p[0] = ('*', p[1], p[3])

def p_expression_binop_divide_and_minus(p):
    '''expression : expression MINUS expression
				| expression DIVIDE expression'''
    if p[2] == '-': p[0] = ('-', p[1], p[3])
    else : p[0] = ('/', p[1], p[3])

def p_expression_binop_bool(p):
    '''expression : expression AND expression
                | expression OR expression
                | expression LOWER expression
                | expression LOWER EGAL expression
                | expression HIGHER expression
                | expression HIGHER EGAL expression'''
    if p[2] == '&':
        p[0] = ('&', p[1], p[3])
    elif p[2] == '|':
        p[0] = ('|', p[1], p[3])
    elif p[2] == '<':
        if p[3] == '=':
            p[0] = ('<=', p[1], p[4])
        else:
            p[0] = ('<', p[1], p[3])
    elif p[2] == '>':
        if p[3] == '=':
            p[0] = ('>=', p[1], p[4])
        else:
            p[0] = ('>', p[1], p[3])

def p_expression_group(p):
    'expression : LPAREN expression RPAREN'
    p[0] = p[2]

def p_expression_number(p):
    'expression : NUMBER'
    p[0] = p[1]

def p_error(p):
    print("Syntax error at '%s'" % p.value)

def evalExpr(t):
    if type(t) is int: return t
    if type(t) is str:
        if t in dicoParams:
            if(dicoParams[t] != None):
                return dicoParams[t]
        if t in dico:
            return dico[t]

    if type(t) is tuple:

        if t[0] == '+':     return evalExpr(t[1]) + evalExpr(t[2])
        if t[0] == '-':     return evalExpr(t[1]) - evalExpr(t[2])
        if t[0] == '*':     return evalExpr(t[1]) * evalExpr(t[2])
        if t[0] == '/':     return evalExpr(t[1]) / evalExpr(t[2])
        if t[0] == '>':     return evalExpr(t[1]) > evalExpr(t[2])
        if t[0] == '<':     return evalExpr(t[1]) < evalExpr(t[2])
        if t[0] == '>=':     return evalExpr(t[1]) >= evalExpr(t[2])
        if t[0] == '<=':     return evalExpr(t[1]) <= evalExpr(t[2])
        if t[0] == '&':     return True if t[1] and t[2] else False
        if t[0] == '|':     return True if t[1] or t[2] else False
    return 'UNK'

def evalInst(t):

    if t == 'empty':
        return

    if t[0] == 'print':
       print(evalExpr(t[1]))

    if t[0] == 'bloc':
        evalInst(t[1])
        evalInst(t[2])

    if t[0] == 'assign':
        dico[t[1]] = evalExpr(t[2])

    if t[0] == 'while':
        while evalExpr(t[1]): #i=0;while(i<10){print(i);i=i+1;};
            evalInst(t[2])

    if t[0] == 'for':
        evalInst(t[1])
        while evalExpr(t[2]): #for(i=0;i<5;i=i+1){print(2);};
            evalInst(t[4])
            evalInst(t[3])

    if t[0] == 'if':
        if(len(t) == 3):
            if evalExpr(t[1]):
                evalInst(t[2])
        else:
            if evalExpr(t[1]):
                evalInst(t[2])
            else:
                evalInst(t[3]) #x=2;if(x<3){print(x);}else{print(6);};

    if t[0] == 'voidFunction':
        dicoFunc[t[1][0]] = t[1][1] #voidFunction carre(){print(2);};for(i=0;i<10;i=i+1){carre();};

    if t[0] == 'function':
        dicoFunc[t[1][0]] = t[1][1] #function toto(x,y,z){print(x+y);};toto(1,8,3);function titi(i,s){print(i+s);};titi(33,66);
        evalInst(t[1][2])

    if t[0] == 'param':
        dicoParams[t[1]] = None
        evalInst(t[2])

    if t[0] == 'args':
        for cle in dicoParams.keys():
            if(dicoParams[cle] == None):
                dicoParams[cle] = evalExpr(t[1])
                break
        evalInst(t[2])

    if t[0] == 'call':
        evalInst(dicoFunc[t[1]])

    if t[0] == 'callParam':
        evalInst(t[2])
        evalInst(dicoFunc[t[1]])
        dicoParams.clear()


yacc.yacc()
dico = {}
dicoFunc = {}
dicoParams = {}
s = input('calc > ')
yacc.parse(s)

#i=0;if(i>3){x=3+3;print(x);};
#for(i=0;i+1<3;i+=1){x=3+3;print(x);};
#i=0;while(i<3){x=3+3;print(x);i=i+1;};
#printString("test");
#1+1;1-1;1*1;1/1;x=1;x=1*1;x++;x--;x+=1;x-=1;x*=1;x/=1;