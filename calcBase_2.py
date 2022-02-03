import ply.yacc as yacc
from genereTreeGraphviz2 import printTreeGraph

reserved = {
    'print': 'PRINT',
    'printString': 'PRINTSTRING',
    'if': 'IF',
    'for': 'FOR',
    'while': 'WHILE',
    'else': 'ELSE',
    'voidFunction': 'VOIDFUNCTION'
}

tokens = [
    'NUMBER','MINUS',
    'PLUS','TIMES','DIVIDE',
    'LPAREN','RPAREN',
    'LACOL','RACOL','DOUBLEC',
    'AND', 'OR', 'SEMICOLON', 'NAME', 'EGAL', 'HIGHER', 'LOWER'
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
    if p[1] == 'for': p[0] = ('for ', p[3], p[5], p[7], p[10])
    if p[1] == 'while': p[0] = ('while ', p[3], p[6])
    if p[1] == 'printString': p[0] = ('print', p[4])

def p_statement_functions(p):
    'statement : VOIDFUNCTION NAME LPAREN RPAREN LACOL bloc RACOL SEMICOLON'
    p[0] = ('voidFunction', (p[2], p[6], 'empty'), 'empty')

def p_statement_call(p):
    'statement : NAME LPAREN RPAREN SEMICOLON'
    p[0] = ('call', p[1], 'empty')

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
    print('eval de', t)
    if type(t) is int: return t
    if type(t) is str:
        return dico[t]
        if t in dicoFunc:
            pass
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
        while evalExpr(t[1]):
            evalInst(t[2])

    if t[0] == 'for':
        evalInst(t[1])
        while evalExpr(t[2]):
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

        dicoFunc[t[1][0]] = t[1][1] #voidFunction toto(){print(2);};
        print('t[1][1] = ', dicoFunc)

    if t[0] == 'call':
        evalInst(dicoFunc[t[1]])


yacc.yacc()
dico = {}
dicoFunc = {}

s = input('calc > ')
yacc.parse(s)

#i=0;if(i>3){x=3+3;print(x);};
#for(i=0;i+1<3;i+=1){x=3+3;print(x);};
#i=0;while(i<3){x=3+3;print(x);i=i+1;};
#printString("test");
#1+1;1-1;1*1;1/1;x=1;x=1*1;x++;x--;x+=1;x-=1;x*=1;x/=1;