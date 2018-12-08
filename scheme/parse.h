



//int alphatable[4]={0,0XF3FFAC72,0XD7FFFFFF,0X57FFFFFF};
int alphatable[4]={0,0XF7FFEC72,0XD7FFFFFF,0X57FFFFFF};
int alpha(char c){
	return (alphatable[c/32]&(1<<(c%32)))!=0;
}
char* lexA(char* s){
	if(alpha(*s))return lexA(s+1);
	else return s;
}
char* jmpspace(char* s){
	if(isspace(*s))return jmpspace(s+1);
	else return s;
}

char* copystr(char* s,int len){
	char* r=(char*)malloc((len+1)*sizeof(char));
	memcpy(r,s,len);
	r[len]=0;
	return r;
}

int lextype(char* s){
	switch(*s){
	case '(':return 1;
	case ')':return 2;
	case '.':return 3;
	default:return 0;
	}
}

char* getsense(char* str,FILE* fp,char* CODE){
	str=jmpspace(str);
	if(*str!=0)return str;
	else if(feof(fp))return NULL;
	else{
		fgets(CODE,200,fp);
		return getsense(CODE,fp,CODE);
	}
}

	
char* parseL(char*,var*,FILE* fp,char*);
char* parseS(char* str,var* v,FILE* fp,char* CODE){
	char* ss;
	str=getsense(str,fp,CODE);
	switch(lextype(str)){
	case 0:

		ss=lexA(str);
		if(isdigit(*str))*v=makeint(atoi(str));
		else *v=makestr(copystr(str,ss-str));
		return ss;
	case 1:
		ss=parseL(str+1,v,fp,CODE);		
		return ss+1;
	default:return NULL;
	}
}
char* parseL(char* str,var* v,FILE* fp,char* CODE){
	var r1,r2;
	str=getsense(str,fp,CODE);
	switch(lextype(str)){
	case 0:
	case 1:
		str=parseS(str,&r1,fp,CODE);
		str=parseL(str,&r2,fp,CODE);
		*v=cons(r1,r2);
		return str;
	case 2:
		*v=NIL;
		return str;
	case 3:return parseS(str+1,v,fp,CODE);
	default:return NULL;
	}
}
var eval(var,var);
void _runfile(char* code,var env,FILE* fp,char* CODE){
	char* code1;
	var exp;
	printf("> ");
	code=getsense(code,fp,CODE);
	if(code!=NULL){
	    
		code1=parseS(code,&exp,fp,CODE);
		display(eval(exp,env));
		putchar('\n');
		_runfile(code1,env,fp,CODE);
	}
}
void runfile(var env,FILE* fp){
	char* CODE=(char*)malloc(200);
	CODE[0]=0;
	_runfile(CODE,env,fp,CODE);
	free(CODE);
}


/*
s->a|(L)
L->SL|e
	a	(	)
S	1	2
L	1	1	2
*/
