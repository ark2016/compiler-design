```
1) WHITESPACE : [ \t\n]+
2) ID         : [A-Za-z][A-Za-z0-9]*
3) INT        : [0-9]+
4) open       : open
   close      : close
5) <<         : <<
   >>         : >>
6) STRING     : ' ( (\'|[^'])* ) '
7) EOF        : EOF
```

```dot
digraph finite_state_machine {
    rankdir=LR;  
    node [shape = circle];
    start [shape=point]; 
    start -> q0 [label="", arrowhead="normal"]; 
    
    q0 [label="S"];
    q1 [label="WHITESPACE", shape=doublecircle];
    q2 [label="ID", shape=doublecircle];
    q3 [label="INT", shape=doublecircle];
    q5 [label="<<", shape=doublecircle];
    q7 [label=">>", shape=doublecircle];
    q11 [label="open", shape=doublecircle];
    q16 [label="close", shape=doublecircle];
    q19 [label="строковые литералы", shape=doublecircle];
    q20 [label="EOF", shape=doublecircle];
    
    q8[shape=doublecircle];
    q9[shape=doublecircle];
    q10[shape=doublecircle];
    q12[shape=doublecircle];
    q13[shape=doublecircle];
    q14[shape=doublecircle];
    q15[shape=doublecircle];
    
    q0 -> q1 [label="\" \"\\t\\n"];
    q0 -> q2 [label="A-Za-z^{o,c}"];
    q0 -> q3 [label="0-9"];
    q0 -> q4 [label="<"];
    q0 -> q6 [label=">"];
    q0 -> q8 [label="o"];
    q0 -> q17 [label="'"];
    q0 -> q20 [label="EOF"];
    

    q2 -> q2 [headport="ne", tailport="n", label="A-Za-z0-9"];
    
    q3 -> q3 [headport="ne", tailport="n", label="0-9"];
    
    q4 -> q5 [label="<"];
    
    q6 -> q7 [label=">"];
    
    q8 -> q9 [label="p"];
    q8 -> q2 [label="A-Za-z0-9"];
    
    q9 -> q10 [label="e"];
    q9 -> q2 [label="A-Za-z0-9"];
    
    q10 -> q11 [label="n"];
    q10 -> q2 [label="A-Za-z0-9"];
    
    q11 -> q2 [label="A-Za-z0-9"];
    
    q0 -> q12 [label="c"];
    q12 -> q2 [label="A-Za-z0-9"];
    
    q12 -> q13 [label="l"];
    q13 -> q2 [label="A-Za-z0-9"];
    
    q13 -> q14 [label="o"];
    q14 -> q2 [label="A-Za-z0-9"];
    
    q14 -> q15 [label="s"];
    q15 -> q2 [label="A-Za-z0-9"];
    
    q15 -> q16 [label="e"];
    q16 -> q2 [label="A-Za-z0-9"];
    
    q17 -> q17 [label="^'^\\"];
    
    q17 ->  q18 [label="\\"];
    
    q18 ->  q17 [label="."];
    
    q17 -> q19 [label="'"];
}

```
