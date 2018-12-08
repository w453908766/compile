typedef struct{
	int type;
	void* data;
}var;
#define type(v) ((v).type)
#define data(v) ((v).data)
#define makevar(t,d) _makevar((t),(void*)(d))
var _makevar(int t,void* d){
	var v;
	type(v)=t;
	data(v)=d;
	return v;
}
var NIL={0,NULL};

enum VARTYPE{VPAIR,VSTR,VINT,VPVV,VPOT};
#define ispot(v) (type(v)==VPOT)
#define getpot(v) ((var*)data(v))
#define makepot(x) makevar(VPOT,(x))


#define isatom(v) (type(v)!=VPAIR)
#define BNIL(v) (type(v)==VPAIR&&data(v)==NULL)

#define isstr(v) (type(v)==VSTR)
#define getstr(v) ((char*)data(v))
#define makestr(x) makevar(VSTR,(x))
int strequ(var v1,var v2){return strcmp(getstr(v1),getstr(v2))==0;}

#define isint(v) (type(v)==VINT)
#define getint(v) ((int)data(v))
#define makeint(x) makevar(VINT,(x))
int varequ(var v1,var v2){return type(v1)==type(v2)&&data(v1)==data(v2);}

#define isvv(v) (type(v)==VPVV)
#define getvv(v) ((vv)data(v))
#define makevv(x) makevar(VPVV,(x))
void putatom(var v){
	if(isstr(v))printf("%s",getstr(v));
	else if(isint(v))printf("%d",getint(v));
	else if(ispot(v))printf("%d",getint(v));
	else if(isvv(v))printf("<VV>");
	else return;
}