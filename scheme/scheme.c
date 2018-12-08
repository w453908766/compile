#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<ctype.h>
#include"var.h"
#include"pair.h"
#include"parse.h"
#include"ppcd.h"
#include"key.h"


void main(){
	var globenv;
	globenv=initenv();	
	runfile(globenv,stdin);

}

