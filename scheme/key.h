typedef var (*vvv)(var,var);
var quote(var,var);
var _if(var,var);
var lambda(var,var);
var listofvalues(var,var);
var _include(var,var);
var begin(var,var);
var apply(var,var);
var eval(var,var);
var makeenv(var,var,var);
var define(var,var);
var scan(var,var,var);
var findvar(var,var);
var strarrtolist(char**,int);
var vvarrtolist(vv*,int);

char* keyword[]={"apply","begin","define","if","include","lambda","quote"};
vvv keyop[]={apply,begin,define,_if,_include,lambda,quote};




var _include(var exp,var env){
	FILE* fp;
	fp=fopen(getstr(car(exp)),"r");
	runfile(env,fp);
	fclose(fp);
	return NIL;
}



var begin(var exp,var env){
	if(BNIL(cdr(exp)))return eval(car(exp),env);
	else{
		eval(car(exp),env);
		return begin(cdr(exp),env);
	}
}
var quote(var exp,var env){return car(exp);}
var _if(var exp,var env){
	if(getint(eval(car(exp),env))!=0)return eval(cadr(exp),env);
	else return eval(caddr(exp),env);
}
var lambda(var exp,var env){
	return cons(exp,env);
}

var listofvalues(var exps,var env){
	if(BNIL(exps))return NIL;
	else return cons(eval(car(exps),env),listofvalues(cdr(exps),env));
}


var apply(var exp,var env){
	var body,argument,penv;
	var optor=eval(car(exp),env);
	var opand=listofvalues(cdr(exp),env);
	if(isvv(optor))return (getvv(optor))(opand);
	else{
		body=cdr(car(optor));
		argument=car(car(optor));
		penv=cdr(optor);
		return begin(body,makeenv(argument,opand,penv));
	}
}



var eval(var exp,var env){
	char** k;
	char* s;

	if(isatom(exp)){
		if(isint(exp))return exp;
		else return findvar(exp,env);
	}
	else{
		s=getstr(car(exp));
		k=(char**)bsearch(&s,keyword,sizeof(keyword)/sizeof(char*),sizeof(char*),cmp);
		if(k!=NULL)return (keyop[k-keyword])(cdr(exp),env);
		else return apply(exp,env);
	}
}

var makeenv(var vars,var vals,var baseenv){
	return cons(cons(vars,vals),baseenv);
}
var define(var exp,var env){
	cdr(car(env))=cons(eval(cadr(exp),env),cdr(car(env)));
	car(car(env))=cons(car(exp),car(car(env)));
	return NIL;
}

var scan(var vars,var vals,var v){

	if(BNIL(vars))return NIL;
	else if(isstr(vars)){
		if(strequ(vars,v))return vals;
		else return NIL;
	}
	else if(strequ(car(vars),v))return car(vals);
	else return scan(cdr(vars),cdr(vals),v);
	
}

var findvar(var v,var env){
	var val;
	if(BNIL(env))return NIL;
	else{
		val=scan(car(car(env)),cdr(car(env)),v);
		if(!BNIL(val))return val;
		else return findvar(v,cdr(env));
	}
}

var initenv(){
	var vars,vals;
	var lpcd,lpcdop;
	lpcd=strarrtolist(ppcd,sizeof(ppcd)/sizeof(char*));
	lpcdop=vvarrtolist(ppcdop,sizeof(ppcdop)/sizeof(vv));
	vars=cons(makestr("nil"),lpcd);
	vals=cons(NIL,lpcdop);
	return makeenv(vars,vals,NIL);
}






