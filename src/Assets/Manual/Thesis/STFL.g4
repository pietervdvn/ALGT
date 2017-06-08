grammar STFL;

bool	: 'True'
	| 'False';

baseType	: 'Int'
		| 'Bool' ;

typeTerm	: baseType
		| '(' baseType ')';

type	: typeTerm '->' type
	| typeTerm
	;

ID	: [a-z]+ ;

INT	: '0'..'9'+;

value	: bool
	| INT;

e	: eL '+' e
	| eL '::' type 
	| eL e
	| eL ;

eL	: value 
	| ID 
	| '(' '\\' ID ':' type '.' e ')' 
	| 'If' e 'Then' e 'Else' e 
	| '(' e ')';

WS : [\t\r\n ]+ -> skip;

