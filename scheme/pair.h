

typedef struct NPAIR *pair;
typedef struct NPAIR PAIR;
struct NPAIR{
	var car;
	var cdr;
};
#define car(v) (getpair(v)->car)
#define cdr(v) (getpair(v)->cdr)
#define cadr(v) car(cdr(v))
#define caddr(v) car(cdr(cdr(v)))





var cons(var a,var d){
	pair z=(pair)malloc(sizeof(PAIR));
	z->car=a;
	z->cdr=d;
	return makevar(VPAIR,z);
}
#define ispair(v) (type(v)==VPAIR)
#define getpair(v) ((pair)data(v)) 

int _display(var v,int b,int s){
	int s1;
	if(isatom(v)){
		if(s==1)putchar(' ');
		putatom(v);
		return 1;
	}
	else if(BNIL(v))return s;
	else if(b==0){
			if(s==1)putchar(' ');
			putchar('(');
			s1=_display(car(v),0,0);
			if(isatom(cdr(v)))printf(" .");//
			_display(cdr(v),1,s1);
			putchar(')');
			return 1;
	}
	else{
			s1=_display(car(v),0,s);
			if(isatom(cdr(v)))printf(" .");
			return _display(cdr(v),1,s1);
	}
	
}
void display(var v){
	_display(v,0,0);
	putchar(' ');

}
