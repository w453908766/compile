

typedef var (*vv)(var);
#define BOP(item,s)  makeint(getint(car(item)) s getint(cadr(item)))
var equ(var item){
	return makeint(varequ(car(item),cadr(item)));
}


var add(var item){
	var a,b;
	int va,vb;
	a=car(item);
	b=cadr(item);
	va=getint(a);
	vb=getint(b);
	switch((isint(a)<<1)|isint(b)){
	case 3:return makeint(va+vb);
	case 2:return makepot(vb+(va<<3));
	case 1:return makepot(va+(vb<<3));
	default:return makepot(va+vb);
	}
}
var sub(var item){
	var a,b;
	int va,vb;
	a=car(item);
	va=getint(a);

	if(BNIL(cdr(item)))return makeint(-va);
	b=cadr(item);
	vb=getint(b);
	switch((isint(a)<<1)|isint(b)){
	case 3:return makeint(va-vb);
	case 1:return makepot(va-(vb<<3));
	case 0:return makeint((va-vb)>>3);
	default:return makepot(va+vb);
	}
}
var mul(var item){return BOP(item,*);}
var div1(var item){return BOP(item,/);}
var mod(var item){return BOP(item,%);}
var more(var item){return BOP(item,>);}
var less(var item){return BOP(item,<);}
var and(var item){return BOP(item,&&);}
var or(var item){return BOP(item,||);}
var not(var item){return makeint(!getint(car(item)));}
var _car(var item){return car(car(item));}
var _cdr(var item){return cdr(car(item));}
var _cons(var item){return cons(car(item),cadr(item));}
var display1(var item){display(car(item));return NIL;}

var makevec(var item){return makepot(malloc(getint(car(item))*sizeof(var)));}
var set(var item){
	*getpot(car(item))=cadr(item);
	return NIL;
}
var val(var item){
	return *getpot(car(item));
}
void display(var);
void putvararr(var* arr,int n){
	if(n==0)putchar('\n');
	else{
		display(arr[0]);
		putvararr(arr+1,n-1);
	}
}


char* ppcd[]={"%","*","+","-","/","<","=",">","and","car","cdr","cons","not","or","display","vec","set","val"};
vv ppcdop[]={mod,mul,add,sub,div1,less,equ,more,and,_car,_cdr,_cons,not,or,display1,makevec,set,val};


int cmp(const void* a,const void* b){
	return strcmp(*(char**)a,*(char**)b);
}

void putarr(char** arr,int n){
	if(n==0)putchar('\n');
	else{
		puts(arr[0]);
		putarr(arr+1,n-1);
	}
}

var strarrtolist(char** strarr,int n){
	if(n==0)return NIL;
	else return cons(makestr(strarr[0]),strarrtolist(strarr+1,n-1));
}

var vvarrtolist(vv* vvarr,int n){
	if(n==0)return NIL;
	else return cons(makevv(vvarr[0]),vvarrtolist(vvarr+1,n-1));
}




