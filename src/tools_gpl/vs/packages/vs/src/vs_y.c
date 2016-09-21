#ifndef lint
static char const 
yyrcsid[] = "$FreeBSD: src/usr.bin/yacc/skeleton.c,v 1.28 2000/01/17 02:04:06 bde Exp $";
#endif
#include <stdlib.h>
#define YYBYACC 1
#define YYMAJOR 1
#define YYMINOR 9
#define YYLEX yylex()
#define YYEMPTY -1
#define yyclearin (yychar=(YYEMPTY))
#define yyerrok (yyerrflag=0)
#define YYRECOVERING() (yyerrflag!=0)
static int yygrowstack();
#define YYPREFIX "yy"
#line 32 "vs_y.y"
static char rcsid[] = "$Id: vs.y,v 1.2 1997/11/18 13:10:32 mooiman Exp $";

/* include files standaard */

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <float.h>

#include "btps.h"
#include "au.h"
#include "fu.h"
#include "wr.h"
#include "vr.h"
/*
#include "st.h"
#include "gen.h"
#include "ex.h"
#include "fm.h"
#include "fu.h"
#include "gr.h"
#include "sm.h"
*/

/*
 * Global variable
 */
extern BInt4 neffds;

void FU_intrinsic ( BInt2   , const BText   , const BText   , BRea8 * );
extern int isatty( int );

/* variabelen die gedeeld worden met de lexical analyser */
double fl_value;
int int_value;
extern char * yytext;    /* lexer text buffer      */
extern int yylex(void);
char *promptstring = ">>";

int ret_val;

static int    grp_uindex[5][3];
static int    elm_uindex[5][3];
static int    lindex[3];
static char * varnms[MAXNUMBEROFVARS+1];
static char * parnms[MAXNUMBEROFPARS+1];
static int    grp_dimcnt=0;
static int    elm_dimcnt=0;
static int    indcnt=0;
static int    varcnt=0;
static int    parcnt=0;


void yyerror (char * s)
{
    (void)fprintf (stderr, "%s\n", s ) ;
    if (!isatty(0)) {
	/* input from file, so exit */
	exit (1);
    }
}

#line 100 "vs_y.y"
typedef union {
    int    ival;
    float  fval;
    char * string;
    } YYSTYPE;
#line 87 "vs_y.c"
#define YYERRCODE 256
#define FILE_END 0
#define K_DEF_ 257
#define K_DELE 258
#define K_DISP 259
#define K_EQUA 260
#define K_FROM 261
#define K_INDI 262
#define K_LET_ 263
#define K_MACR 264
#define K_MEMO 265
#define K_MODI 266
#define K_OFF_ 267
#define K_ON__ 268
#define K_ORDE 269
#define K_PLAY 270
#define K_RECO 271
#define K_RELE 272
#define K_STAT 273
#define K_TO__ 274
#define K_TYPE 275
#define K_USE_ 276
#define K_WRIT 277
#define K_EOF_ 278
#define K_QUIT 279
#define K_HELP 280
#define K_SHEL 281
#define K_LBRC 282
#define K_RBRC 283
#define K_COMM 284
#define K_INTG 285
#define K_SEMI 286
#define K_SIGN 287
#define K_EXEC 288
#define K_TR2W 289
#define K_MAXM 290
#define K_MINM 291
#define K_TRAG 292
#define IDNTFR 293
#define K_FLOA 294
#define K_WITH 295
#define K_AVG_ 296
#define K_PARM 297
#define K_RETN 298
#define K_ALL_ 299
#define UNKNOWN 300
#define STAT_END 301
const short yylhs[] = {                                        -1,
    1,    3,    4,    5,    6,    0,    0,    8,    8,    8,
    8,    8,    8,    8,    8,    8,    8,    8,    8,    9,
    9,   10,   10,   10,   10,   11,   11,   11,   11,   11,
   11,   11,   11,   11,   11,   11,   11,   11,   11,   11,
   12,   12,   12,   13,   13,   14,   14,   14,   14,   14,
   14,   14,   14,   19,   19,   20,   20,   15,    7,   17,
   17,   18,   18,   22,   22,   21,   21,   23,   23,    2,
    2,   16,
};
const short yylen[] = {                                         2,
    1,    1,    1,    1,    1,    1,    2,    1,    1,    1,
    1,    1,    1,    1,    1,    1,    1,    1,    2,    2,
    5,    3,    5,    3,    3,    8,    7,    7,    7,    6,
    7,    7,    6,    7,    7,    6,    7,    7,    5,    5,
    3,    5,    6,    3,    3,    3,    4,    5,    6,    5,
    6,    7,    8,    1,    2,    1,    2,    2,    2,    3,
    0,    4,    1,    1,    3,    1,    3,    1,    3,    0,
    1,    2,
};
const short yydefred[] = {                                      0,
    0,   18,    0,    0,    0,    0,    0,    0,    0,    4,
    0,   17,    0,    0,   15,    6,    8,    9,   10,   11,
   12,   13,   14,   16,   19,    0,    0,    3,    0,    0,
    0,    0,   20,    0,   54,    0,   72,   58,    0,    7,
   59,   24,    0,   22,   25,    0,   45,   44,    0,    0,
   41,   55,    0,    0,    0,   46,    0,    1,    0,    0,
    2,    0,    0,    0,    0,    0,    0,    0,   56,    0,
   47,   23,    0,    0,    0,   39,   40,    0,    5,    0,
    0,   21,   42,    0,    0,    0,   48,    0,   50,   57,
   30,    0,    0,   33,    0,    0,   36,    0,    0,   71,
   68,    0,    0,    0,    0,    0,    0,   43,    0,   49,
   51,   31,   32,   34,   35,   37,   38,   60,    0,    0,
   29,   28,   27,    0,    0,   52,    0,   69,    0,   63,
   26,   53,    0,    0,    0,    0,   62,    0,
};
const short yydgoto[] = {                                      13,
  100,  101,   64,   35,   14,   80,   15,   16,   17,   18,
   19,   20,   21,   22,   23,   24,   81,  131,   36,   70,
  102,  133,  103,
};
const short yysindex[] = {                                      1,
 -280,    0, -257, -230, -221, -281, -230, -247, -237,    0,
 -230,    0,    1, -233,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0, -226, -263,    0, -224, -172,
 -197, -194,    0, -148,    0, -268,    0,    0, -212,    0,
    0,    0, -230,    0,    0, -220,    0,    0, -230, -230,
    0,    0, -230, -230, -190,    0, -188,    0, -230, -230,
    0, -230, -186, -184, -231, -182, -241, -254,    0, -279,
    0,    0, -270, -244, -239,    0,    0, -175,    0, -276,
 -149,    0,    0, -180, -230, -179,    0, -178,    0,    0,
    0, -177, -176,    0, -174, -173,    0, -171, -170,    0,
    0, -191, -168, -169, -167, -166, -230,    0, -261,    0,
    0,    0,    0,    0,    0,    0,    0,    0, -175, -175,
    0,    0,    0, -275, -165,    0, -168,    0, -175,    0,
    0,    0, -189, -168, -164, -175,    0, -168,
};
const short yyrindex[] = {                                      0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0, -147,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0, -193,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0, -187,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0, -193, -193,
    0,    0,    0,    0,    0,    0, -185,    0, -193,    0,
    0,    0,    0, -183,    0, -193,    0, -181,
};
const short yygindex[] = {                                      0,
  -46,   -2,    7,   -1,    0,    0,    0,  107,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,   73,   44,
    0,    0, -106,
};
#define YYTABLESIZE 302
const short yytable[] = {                                      63,
    2,   29,   30,   32,   34,   50,  129,   26,   58,   39,
   43,   28,  127,   28,   58,   27,   28,   61,   88,   33,
   25,   89,  134,   61,   28,  130,   92,   95,   98,  138,
   91,   28,   51,  104,   52,   28,  125,   44,   28,  126,
   58,   57,   85,   86,   65,   58,   87,   66,   67,   61,
   78,   28,   69,   37,   61,   79,   94,   73,   74,   83,
   75,   97,   28,   38,   58,   84,   52,   41,   90,   59,
   60,   28,   28,   61,   42,   62,   45,   31,  106,   93,
   96,   99,   53,   69,   54,   55,  105,   46,   56,   70,
   70,  118,   70,  135,  119,   66,  136,   67,   66,   64,
   67,   65,   64,   47,   65,  124,   48,   90,   49,   58,
   71,  107,   72,   61,   76,  120,   77,  128,   82,   40,
  108,  110,  111,  112,  113,   68,  114,  115,  109,  116,
  117,  121,    0,  122,  123,  132,  137,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    1,    0,    0,    3,
    0,    0,    0,    4,    0,    0,    0,    0,    0,    0,
    0,    0,    5,    0,    0,    0,    6,    7,    0,    8,
    9,   10,    0,    0,    0,    0,    0,    0,   11,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,   12,
};
const short yycheck[] = {                                      46,
    0,    3,    4,    5,    6,  274,  282,  265,  285,   11,
  274,  293,  119,  293,  285,  273,  293,  294,  298,  301,
  301,  301,  129,  294,  293,  301,   73,   74,   75,  136,
  301,  293,  301,   80,   36,  293,  298,  301,  293,  301,
  285,   43,  297,  298,   46,  285,  301,   49,   50,  294,
  282,  293,   54,  301,  294,  287,  301,   59,   60,  301,
   62,  301,  293,  301,  285,   67,   68,  301,   70,  290,
  291,  293,  293,  294,  301,  296,  301,  299,   80,   73,
   74,   75,  295,   85,  297,  298,   80,  260,  301,  283,
  284,  283,  286,  283,  286,  283,  286,  283,  286,  283,
  286,  283,  286,  301,  286,  107,  301,  109,  257,  285,
  301,  261,  301,  261,  301,  284,  301,  120,  301,   13,
  301,  301,  301,  301,  301,   53,  301,  301,   85,  301,
  301,  301,   -1,  301,  301,  301,  301,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  256,   -1,   -1,  259,
   -1,   -1,   -1,  263,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  272,   -1,   -1,   -1,  276,  277,   -1,  279,
  280,  281,   -1,   -1,   -1,   -1,   -1,   -1,  288,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  301,
};
#define YYFINAL 13
#ifndef YYDEBUG
#define YYDEBUG 0
#endif
#define YYMAXTOKEN 301
#if YYDEBUG
const char * const yyname[] = {
"end-of-file",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,"K_DEF_","K_DELE","K_DISP",
"K_EQUA","K_FROM","K_INDI","K_LET_","K_MACR","K_MEMO","K_MODI","K_OFF_",
"K_ON__","K_ORDE","K_PLAY","K_RECO","K_RELE","K_STAT","K_TO__","K_TYPE",
"K_USE_","K_WRIT","K_EOF_","K_QUIT","K_HELP","K_SHEL","K_LBRC","K_RBRC",
"K_COMM","K_INTG","K_SEMI","K_SIGN","K_EXEC","K_TR2W","K_MAXM","K_MINM",
"K_TRAG","IDNTFR","K_FLOA","K_WITH","K_AVG_","K_PARM","K_RETN","K_ALL_",
"UNKNOWN","STAT_END",
};
const char * const yyrule[] = {
"$accept : statements",
"i_intg : K_INTG",
"i_floa : K_FLOA",
"idntfr : IDNTFR",
"i_shel : K_SHEL",
"i_sign : K_SIGN",
"statements : statement",
"statements : statements statement",
"statement : use_statement",
"statement : disp_statement",
"statement : let_statement",
"statement : write_statement",
"statement : relea_statement",
"statement : exec_statement",
"statement : help_statement",
"statement : shell_statement",
"statement : quit_statement",
"statement : STAT_END",
"statement : FILE_END",
"statement : error STAT_END",
"use_statement : K_USE_ STAT_END",
"use_statement : K_USE_ idntfr K_DEF_ idntfr STAT_END",
"disp_statement : K_DISP K_STAT STAT_END",
"disp_statement : K_DISP K_STAT K_TO__ idntfr STAT_END",
"disp_statement : K_DISP K_MEMO STAT_END",
"disp_statement : K_DISP idntfr STAT_END",
"let_statement : K_LET_ idntfr K_EQUA idntfr indelm K_FROM idntfr indstat",
"let_statement : K_LET_ idntfr K_EQUA idntfr i_sign idntfr STAT_END",
"let_statement : K_LET_ idntfr K_EQUA idntfr i_sign i_floa STAT_END",
"let_statement : K_LET_ idntfr K_EQUA idntfr i_sign i_intg STAT_END",
"let_statement : K_LET_ idntfr K_EQUA K_MAXM idntfr STAT_END",
"let_statement : K_LET_ idntfr K_EQUA K_MAXM idntfr i_intg STAT_END",
"let_statement : K_LET_ idntfr K_EQUA K_MAXM idntfr i_floa STAT_END",
"let_statement : K_LET_ idntfr K_EQUA K_MINM idntfr STAT_END",
"let_statement : K_LET_ idntfr K_EQUA K_MINM idntfr i_intg STAT_END",
"let_statement : K_LET_ idntfr K_EQUA K_MINM idntfr i_floa STAT_END",
"let_statement : K_LET_ idntfr K_EQUA K_AVG_ idntfr STAT_END",
"let_statement : K_LET_ idntfr K_EQUA K_AVG_ idntfr i_intg STAT_END",
"let_statement : K_LET_ idntfr K_EQUA K_AVG_ idntfr i_floa STAT_END",
"let_statement : K_LET_ idntfr K_EQUA i_intg STAT_END",
"let_statement : K_LET_ idntfr K_EQUA i_floa STAT_END",
"write_statement : K_WRIT varnames STAT_END",
"write_statement : K_WRIT varnames K_TO__ idntfr STAT_END",
"write_statement : K_WRIT varnames K_TO__ idntfr idntfr STAT_END",
"relea_statement : K_RELE idntfr STAT_END",
"relea_statement : K_RELE K_ALL_ STAT_END",
"exec_statement : K_EXEC idntfr STAT_END",
"exec_statement : K_EXEC idntfr K_RETN STAT_END",
"exec_statement : K_EXEC idntfr K_WITH varnames STAT_END",
"exec_statement : K_EXEC idntfr K_WITH varnames K_RETN STAT_END",
"exec_statement : K_EXEC idntfr K_PARM parnames STAT_END",
"exec_statement : K_EXEC idntfr K_PARM parnames K_RETN STAT_END",
"exec_statement : K_EXEC idntfr K_WITH varnames K_PARM parnames STAT_END",
"exec_statement : K_EXEC idntfr K_WITH varnames K_PARM parnames K_RETN STAT_END",
"varnames : idntfr",
"varnames : varnames idntfr",
"parnames : idntfr",
"parnames : parnames idntfr",
"help_statement : K_HELP STAT_END",
"shell_statement : i_shel STAT_END",
"indelm : K_LBRC elm_indices K_RBRC",
"indelm :",
"indstat : K_LBRC grp_indices K_RBRC STAT_END",
"indstat : STAT_END",
"grp_indices : indp",
"grp_indices : grp_indices K_SEMI indp",
"elm_indices : indp",
"elm_indices : elm_indices K_SEMI indp",
"indp : ind",
"indp : indp K_COMM ind",
"ind :",
"ind : i_intg",
"quit_statement : K_QUIT STAT_END",
};
#endif
#if YYDEBUG
#include <stdio.h>
#endif
#ifdef YYSTACKSIZE
#undef YYMAXDEPTH
#define YYMAXDEPTH YYSTACKSIZE
#else
#ifdef YYMAXDEPTH
#define YYSTACKSIZE YYMAXDEPTH
#else
#define YYSTACKSIZE 10000
#define YYMAXDEPTH 10000
#endif
#endif
#define YYINITSTACKSIZE 200
int yydebug;
int yynerrs;
int yyerrflag;
int yychar;
short *yyssp;
YYSTYPE *yyvsp;
YYSTYPE yyval;
YYSTYPE yylval;
short *yyss;
short *yysslim;
YYSTYPE *yyvs;
int yystacksize;
/* allocate initial stack or double stack size, up to YYMAXDEPTH */
static int yygrowstack()
{
    int newsize, i;
    short *newss;
    YYSTYPE *newvs;

    if ((newsize = yystacksize) == 0)
        newsize = YYINITSTACKSIZE;
    else if (newsize >= YYMAXDEPTH)
        return -1;
    else if ((newsize *= 2) > YYMAXDEPTH)
        newsize = YYMAXDEPTH;
    i = yyssp - yyss;
    newss = yyss ? (short *)realloc(yyss, newsize * sizeof *newss) :
      (short *)malloc(newsize * sizeof *newss);
    if (newss == NULL)
        return -1;
    yyss = newss;
    yyssp = newss + i;
    newvs = yyvs ? (YYSTYPE *)realloc(yyvs, newsize * sizeof *newvs) :
      (YYSTYPE *)malloc(newsize * sizeof *newvs);
    if (newvs == NULL)
        return -1;
    yyvs = newvs;
    yyvsp = newvs + i;
    yystacksize = newsize;
    yysslim = yyss + newsize - 1;
    return 0;
}

#define YYABORT goto yyabort
#define YYREJECT goto yyabort
#define YYACCEPT goto yyaccept
#define YYERROR goto yyerrlab

#ifndef YYPARSE_PARAM
#if defined(__cplusplus) || __STDC__
#define YYPARSE_PARAM_ARG void
#define YYPARSE_PARAM_DECL
#else	/* ! ANSI-C/C++ */
#define YYPARSE_PARAM_ARG
#define YYPARSE_PARAM_DECL
#endif	/* ANSI-C/C++ */
#else	/* YYPARSE_PARAM */
#ifndef YYPARSE_PARAM_TYPE
#define YYPARSE_PARAM_TYPE void *
#endif
#if defined(__cplusplus) || __STDC__
#define YYPARSE_PARAM_ARG YYPARSE_PARAM_TYPE YYPARSE_PARAM
#define YYPARSE_PARAM_DECL
#else	/* ! ANSI-C/C++ */
#define YYPARSE_PARAM_ARG YYPARSE_PARAM
#define YYPARSE_PARAM_DECL YYPARSE_PARAM_TYPE YYPARSE_PARAM;
#endif	/* ANSI-C/C++ */
#endif	/* ! YYPARSE_PARAM */

int
yyparse (YYPARSE_PARAM_ARG)
    YYPARSE_PARAM_DECL
{
    register int yym, yyn, yystate;
#if YYDEBUG
    register const char *yys;

    if ((yys = getenv("YYDEBUG")))
    {
        yyn = *yys;
        if (yyn >= '0' && yyn <= '9')
            yydebug = yyn - '0';
    }
#endif

    yynerrs = 0;
    yyerrflag = 0;
    yychar = (-1);

    if (yyss == NULL && yygrowstack()) goto yyoverflow;
    yyssp = yyss;
    yyvsp = yyvs;
    *yyssp = yystate = 0;

yyloop:
    if ((yyn = yydefred[yystate])) goto yyreduce;
    if (yychar < 0)
    {
        if ((yychar = yylex()) < 0) yychar = 0;
#if YYDEBUG
        if (yydebug)
        {
            yys = 0;
            if (yychar <= YYMAXTOKEN) yys = yyname[yychar];
            if (!yys) yys = "illegal-symbol";
            printf("%sdebug: state %d, reading %d (%s)\n",
                    YYPREFIX, yystate, yychar, yys);
        }
#endif
    }
    if ((yyn = yysindex[yystate]) && (yyn += yychar) >= 0 &&
            yyn <= YYTABLESIZE && yycheck[yyn] == yychar)
    {
#if YYDEBUG
        if (yydebug)
            printf("%sdebug: state %d, shifting to state %d\n",
                    YYPREFIX, yystate, yytable[yyn]);
#endif
        if (yyssp >= yysslim && yygrowstack())
        {
            goto yyoverflow;
        }
        *++yyssp = yystate = yytable[yyn];
        *++yyvsp = yylval;
        yychar = (-1);
        if (yyerrflag > 0)  --yyerrflag;
        goto yyloop;
    }
    if ((yyn = yyrindex[yystate]) && (yyn += yychar) >= 0 &&
            yyn <= YYTABLESIZE && yycheck[yyn] == yychar)
    {
        yyn = yytable[yyn];
        goto yyreduce;
    }
    if (yyerrflag) goto yyinrecovery;
#if defined(lint) || defined(__GNUC__)
    goto yynewerror;
#endif
yynewerror:
    yyerror("syntax error");
#if defined(lint) || defined(__GNUC__)
    goto yyerrlab;
#endif
yyerrlab:
    ++yynerrs;
yyinrecovery:
    if (yyerrflag < 3)
    {
        yyerrflag = 3;
        for (;;)
        {
            if ((yyn = yysindex[*yyssp]) && (yyn += YYERRCODE) >= 0 &&
                    yyn <= YYTABLESIZE && yycheck[yyn] == YYERRCODE)
            {
#if YYDEBUG
                if (yydebug)
                    printf("%sdebug: state %d, error recovery shifting\
 to state %d\n", YYPREFIX, *yyssp, yytable[yyn]);
#endif
                if (yyssp >= yysslim && yygrowstack())
                {
                    goto yyoverflow;
                }
                *++yyssp = yystate = yytable[yyn];
                *++yyvsp = yylval;
                goto yyloop;
            }
            else
            {
#if YYDEBUG
                if (yydebug)
                    printf("%sdebug: error recovery discarding state %d\n",
                            YYPREFIX, *yyssp);
#endif
                if (yyssp <= yyss) goto yyabort;
                --yyssp;
                --yyvsp;
            }
        }
    }
    else
    {
        if (yychar == 0) goto yyabort;
#if YYDEBUG
        if (yydebug)
        {
            yys = 0;
            if (yychar <= YYMAXTOKEN) yys = yyname[yychar];
            if (!yys) yys = "illegal-symbol";
            printf("%sdebug: state %d, error recovery discards token %d (%s)\n",
                    YYPREFIX, yystate, yychar, yys);
        }
#endif
        yychar = (-1);
        goto yyloop;
    }
yyreduce:
#if YYDEBUG
    if (yydebug)
        printf("%sdebug: state %d, reducing by rule %d (%s)\n",
                YYPREFIX, yystate, yyn, yyrule[yyn]);
#endif
    yym = yylen[yyn];
    yyval = yyvsp[1-yym];
    switch (yyn)
    {
case 1:
#line 134 "vs_y.y"
{ yyval.ival = (BInt4)int_value; }
break;
case 2:
#line 137 "vs_y.y"
{ yyval.fval = (float)fl_value; }
break;
case 3:
#line 140 "vs_y.y"
{ if ( yytext[0] == '\'' ) {
			     /* quoted identifier, trailing
				quote already removed in lexer */
			     yyval.string = GEN_tekst ( &yytext[1] );
			   }
			   else {
			     yyval.string = GEN_tekst ( yytext );
			   }
			 }
break;
case 4:
#line 151 "vs_y.y"
{ yyval.string = GEN_tekst ( yytext ); }
break;
case 5:
#line 154 "vs_y.y"
{ yyval.string = GEN_tekst ( yytext ); }
break;
case 6:
#line 158 "vs_y.y"
{
		    (void)fprintf ( stderr, "%s", promptstring );
		  }
break;
case 7:
#line 162 "vs_y.y"
{
		    (void)fprintf ( stderr, "%s", promptstring );
		  }
break;
case 17:
#line 177 "vs_y.y"
{ yyerrok; }
break;
case 18:
#line 184 "vs_y.y"
{ return ( 0 ); }
break;
case 19:
#line 186 "vs_y.y"
{ yyerrok; }
break;
case 20:
#line 190 "vs_y.y"
{
		    FM_close_nefis_files ();
		  }
break;
case 21:
#line 194 "vs_y.y"
{
		    ret_val=FM_open_nefis_files ( yyvsp[-3].string, yyvsp[-1].string );
		    GEN_free ( (char *)yyvsp[-3].string );
		    GEN_free ( (char *)yyvsp[-1].string );
		    if ( ret_val == 0 ) {
		       (void) SM_read_nefis_meta_data ( neffds );
		    }
		  }
break;
case 22:
#line 205 "vs_y.y"
{
		    FM_display_nefis_file_status (NULL);
		  }
break;
case 23:
#line 209 "vs_y.y"
{
		    FM_display_nefis_file_status (yyvsp[-1].string);
		    GEN_free ( (char *)yyvsp[-1].string );
		  }
break;
case 24:
#line 214 "vs_y.y"
{
		    PR_variables_info_print ();
		  }
break;
case 25:
#line 218 "vs_y.y"
{
		    PR_print_group_info ( NULL,
			     GR_find_group_in_chain (
			             SM_get_group_pointer (), yyvsp[-1].string ));
			GEN_free ( (char *)yyvsp[-1].string );
		  }
break;
case 26:
#line 228 "vs_y.y"
{
		    (void)VR_read_var_from_file (
			    yyvsp[-6].string, yyvsp[-4].string, yyvsp[-1].string, elm_uindex, grp_uindex );
		    GEN_free ( (char *)yyvsp[-6].string );
		    GEN_free ( (char *)yyvsp[-4].string );
		    GEN_free ( (char *)yyvsp[-1].string );
		  }
break;
case 27:
#line 236 "vs_y.y"
{
		    (void)FU_basics ( (char *)yyvsp[-3].string, (char *)yyvsp[-1].string,
				      (char *)yyvsp[-5].string, (char *)yyvsp[-2].string );
		    GEN_free ( (char *)yyvsp[-5].string );
		    GEN_free ( (char *)yyvsp[-3].string );
		    GEN_free ( (char *)yyvsp[-2].string );
		    GEN_free ( (char *)yyvsp[-1].string );
		  }
break;
case 28:
#line 245 "vs_y.y"
{
		    double value;
		    value = yyvsp[-1].fval;
		    (void)FU_simple ( (char *)yyvsp[-3].string, (char *)yyvsp[-5].string,
			             &value, (char *)yyvsp[-2].string );
		    GEN_free ( (char *)yyvsp[-5].string );
		    GEN_free ( (char *)yyvsp[-3].string );
		    GEN_free ( (char *)yyvsp[-2].string );
		  }
break;
case 29:
#line 255 "vs_y.y"
{
		    double value;
		    value = yyvsp[-1].ival;
		    (void)FU_simple ( (char *)yyvsp[-3].string, (char *)yyvsp[-5].string,
			             &value, (char *)yyvsp[-2].string );
		    GEN_free ( (char *)yyvsp[-5].string );
		    GEN_free ( (char *)yyvsp[-3].string );
		    GEN_free ( (char *)yyvsp[-2].string );
		  }
break;
case 30:
#line 265 "vs_y.y"
{
		    (void)FU_intrinsic ( MAXIMUM, (char *)yyvsp[-1].string,
				         (char *)yyvsp[-4].string, NULL );
		    GEN_free ( (char *)yyvsp[-4].string );
		    GEN_free ( (char *)yyvsp[-1].string );
		  }
break;
case 31:
#line 272 "vs_y.y"
{
		    double excl_val;
		    excl_val = yyvsp[-1].ival;
		    (void)FU_intrinsic ( MAXIMUM, (char *)yyvsp[-2].string,
					 (char *)yyvsp[-5].string, &excl_val );
		    GEN_free ( (char *)yyvsp[-5].string );
		    GEN_free ( (char *)yyvsp[-2].string );
		  }
break;
case 32:
#line 281 "vs_y.y"
{
		    double excl_val;
		    excl_val = yyvsp[-1].fval;
		    (void)FU_intrinsic ( MAXIMUM, (char *)yyvsp[-2].string,
				         (char *)yyvsp[-5].string, &excl_val );
		    GEN_free ( (char *)yyvsp[-5].string );
		    GEN_free ( (char *)yyvsp[-2].string );
		  }
break;
case 33:
#line 290 "vs_y.y"
{
		    (void)FU_intrinsic ( MINIMUM, (char *)yyvsp[-1].string,
					 (char *)yyvsp[-4].string, NULL );
		    GEN_free ( (char *)yyvsp[-4].string );
		    GEN_free ( (char *)yyvsp[-1].string );
		  }
break;
case 34:
#line 297 "vs_y.y"
{
		    double excl_val;
		    excl_val = yyvsp[-1].ival;
		    (void)FU_intrinsic ( MINIMUM, (char *)yyvsp[-2].string,
					 (char *)yyvsp[-5].string, &excl_val );
		    GEN_free ( (char *)yyvsp[-5].string );
		    GEN_free ( (char *)yyvsp[-2].string );
		  }
break;
case 35:
#line 306 "vs_y.y"
{
		    double excl_val;
		    excl_val = yyvsp[-1].fval;
		    (void)FU_intrinsic ( MINIMUM, (char *)yyvsp[-2].string,
					 (char *)yyvsp[-5].string, &excl_val );
		    GEN_free ( (char *)yyvsp[-5].string );
		    GEN_free ( (char *)yyvsp[-2].string );
		  }
break;
case 36:
#line 315 "vs_y.y"
{
		    (void)FU_intrinsic ( AVERAGE, (char *)yyvsp[-1].string,
					 (char *)yyvsp[-4].string, NULL );
		    GEN_free ( (char *)yyvsp[-4].string );
		    GEN_free ( (char *)yyvsp[-1].string );
		}
break;
case 37:
#line 322 "vs_y.y"
{
		    double excl_val;
		    excl_val = yyvsp[-1].ival;
		    (void)FU_intrinsic ( AVERAGE, (char *)yyvsp[-2].string,
					 (char *)yyvsp[-5].string, &excl_val );
		    GEN_free ( (char *)yyvsp[-5].string );
		    GEN_free ( (char *)yyvsp[-2].string );
		}
break;
case 38:
#line 331 "vs_y.y"
{
		    double excl_val;
		    excl_val = yyvsp[-1].fval;
		    (void)FU_intrinsic ( AVERAGE, (char *)yyvsp[-2].string,
					 (char *)yyvsp[-5].string, &excl_val);
		    GEN_free ( (char *)yyvsp[-5].string );
		    GEN_free ( (char *)yyvsp[-2].string );
		}
break;
case 39:
#line 340 "vs_y.y"
{
		    (void) FU_set_value ( (char *)yyvsp[-3].string, (BRea4) yyvsp[-1].ival );
		    GEN_free ( (char *)yyvsp[-3].string );
		  }
break;
case 40:
#line 345 "vs_y.y"
{
		    (void)FU_set_value ( (char *)yyvsp[-3].string, yyvsp[-1].fval );
		    GEN_free ( (char *)yyvsp[-3].string );
		  }
break;
case 41:
#line 352 "vs_y.y"
{
		    int i;

		    (void) WR_write_variables ( (char *)NULL, (char *)NULL, varnms );

		    i = 0;
		    while ( varnms[i] != NULL ) {
			GEN_free ( varnms[i] );
			i++;
		    }
		  }
break;
case 42:
#line 365 "vs_y.y"
{
		    int i;

		    WR_write_variables ( (char *)yyvsp[-1].string, (char *)NULL,
			                 varnms );

		    i = 0;
		    while ( varnms[i] != NULL ) {
			GEN_free ( varnms[i] );
			i++;
		    }
		    GEN_free ( (char *)yyvsp[-1].string );
		  }
break;
case 43:
#line 380 "vs_y.y"
{
		    int i;

		    WR_write_variables ( (char *)yyvsp[-2].string, (char *)yyvsp[-1].string,
			                 varnms );

		    i = 0;
		    while ( varnms[i] != NULL ) {
			GEN_free ( varnms[i] );
			i++;
		    }
		    GEN_free ( (char *)yyvsp[-2].string );
		    GEN_free ( (char *)yyvsp[-1].string );
		  }
break;
case 44:
#line 397 "vs_y.y"
{
		    VR_release_variable( yyvsp[-1].string );
		    GEN_free ( (char *)yyvsp[-1].string );
		  }
break;
case 45:
#line 403 "vs_y.y"
{
		    (void)VR_release_all_variables ();
		  }
break;
case 46:
#line 409 "vs_y.y"
{
		    (void)EX_process ( yyvsp[-1].string ,0 );
		    GEN_free ( (char *)yyvsp[-1].string );
		  }
break;
case 47:
#line 414 "vs_y.y"
{
		    (void)EX_process ( yyvsp[-2].string ,1 );
		    GEN_free ( (char *)yyvsp[-2].string );
		  }
break;
case 48:
#line 419 "vs_y.y"
{
		    int i;

		    (void)EX_process_with_vars (
			yyvsp[-3].string, varnms, 0 );

		    i = 0;
		    while ( varnms[i] != NULL ) {
			GEN_free ( varnms[i] );
			i++;
		    }
		    GEN_free ( (char *)yyvsp[-3].string );
		  }
break;
case 49:
#line 433 "vs_y.y"
{
		    int i;

		    (void)EX_process_with_vars (
			yyvsp[-4].string, varnms, 1 );

		    i = 0;
		    while ( varnms[i] != NULL ) {
			GEN_free ( varnms[i] );
			i++;
		    }
		    GEN_free ( (char *)yyvsp[-4].string );
		  }
break;
case 50:
#line 447 "vs_y.y"
{
		    int i;

		    (void)EX_process_with_parms ( yyvsp[-3].string, parnms, 0 );

		    i = 0;
		    while ( parnms[i] != NULL ) {
			GEN_free ( parnms[i] );
			i++;
		    }
		    GEN_free ( (char *)yyvsp[-3].string );
		  }
break;
case 51:
#line 460 "vs_y.y"
{
		    int i;

		    (void)EX_process_with_parms ( yyvsp[-4].string, parnms, 1 );

		    i = 0;
		    while ( parnms[i] != NULL ) {
			GEN_free ( parnms[i] );
			i++;
		    }
		    GEN_free ( (char *)yyvsp[-4].string );
		  }
break;
case 52:
#line 473 "vs_y.y"
{
		    int i;

		    (void)EX_process_with_vars_and_parms (
			yyvsp[-5].string, varnms, parnms, 0 );

		    i = 0;
		    while ( parnms[i] != NULL ) {
			GEN_free ( parnms[i] );
			i++;
		    }

		    i = 0;
		    while ( varnms[i] != NULL ) {
			GEN_free ( varnms[i] );
			i++;
		    }
		    GEN_free ( (char *)yyvsp[-5].string );
		  }
break;
case 53:
#line 493 "vs_y.y"
{
		    int i;

		    (void)EX_process_with_vars_and_parms (
			yyvsp[-6].string, varnms, parnms, 1 );

		    i = 0;
		    while ( parnms[i] != NULL ) {
			GEN_free ( parnms[i] );
			i++;
		    }

		    i = 0;
		    while ( varnms[i] != NULL ) {
			GEN_free ( varnms[i] );
			i++;
		    }
		    GEN_free ( (char *)yyvsp[-6].string );
		  }
break;
case 54:
#line 515 "vs_y.y"
{
		    varcnt = 0;
		    varnms[varcnt++] = (char *)yyvsp[0].string;
		    varnms[varcnt  ] = NULL;
		  }
break;
case 55:
#line 522 "vs_y.y"
{
		    if ( varcnt == MAXNUMBEROFVARS ) {
			yyerror ( "syntax error" );
			YYERROR;
		    }
		    else {
			varnms[varcnt++] = (char *)yyvsp[0].string;
			varnms[varcnt  ] = NULL;
		    }
		  }
break;
case 56:
#line 535 "vs_y.y"
{
		    parcnt = 0;
		    parnms[parcnt++] = (char *)yyvsp[0].string;
		    parnms[parcnt  ] = NULL;
		  }
break;
case 57:
#line 542 "vs_y.y"
{
		    if ( parcnt == MAXNUMBEROFPARS ) {
			yyerror ( "syntax error" );
			YYERROR;
		    }
		    else {
			parnms[parcnt++] = (char *)yyvsp[0].string;
			parnms[parcnt  ] = NULL;
		    }
		  }
break;
case 58:
#line 555 "vs_y.y"
{
		    (void)GEN_display_help ();
		  }
break;
case 59:
#line 561 "vs_y.y"
{
		    (void)system ( (char *)++yyval.string );
		    GEN_free ((char *) yyvsp[-1].string);
		  }
break;
case 61:
#line 569 "vs_y.y"
{
		    int i;
		    int j;
		    for ( j=0 ; j<5 ; j++ ) {
			for ( i=0 ; i<3 ; i++ ) {
			    elm_uindex[j][i] = -1;
			}
		    }
		  }
break;
case 63:
#line 582 "vs_y.y"
{
		    int i;
		    int j;
		    for ( j=0 ; j<5 ; j++ ) {
			for ( i=0 ; i<3 ; i++ ) {
			    grp_uindex[j][i] = -1;
			}
		    }
		  }
break;
case 64:
#line 594 "vs_y.y"
{
		    int i;
		    int j;

		    grp_dimcnt=0;
		    for ( i=0 ; i<3 ; i++) {
			grp_uindex[grp_dimcnt][i] = lindex[i];
		    }
		    for ( j=1 ; j<5 ; j++) {
			for ( i=0 ; i<3 ; i++) {
			    grp_uindex[j][i] = -1;
			}
		    }
		  }
break;
case 65:
#line 609 "vs_y.y"
{
		    if ( grp_dimcnt == 4 ) {
			yyerror ( "syntax error" );
			YYERROR;
		    }
		    else {
			int i;

		    	grp_dimcnt++;
		    	for ( i=0 ; i<3 ; i++ ) {
			    grp_uindex[grp_dimcnt][i] = lindex[i]; } }
		  }
break;
case 66:
#line 624 "vs_y.y"
{
		    int i;
		    int j;

		    elm_dimcnt = 0;
		    for ( i=0 ; i<3 ; i++ ) {
			elm_uindex[elm_dimcnt][i] = lindex[i];
		    }
		    for ( j=1 ; j<5 ; j++ ) {
			for ( i=0 ; i<3 ; i++ ) {
			    elm_uindex[j][i] = -1;
			}
		    }
		  }
break;
case 67:
#line 639 "vs_y.y"
{
		    if ( elm_dimcnt == 4 ) {
			yyerror ( "syntax error" );
			YYERROR;
		    }
		    else {
			int i;

		    	elm_dimcnt++;
		    	for ( i=0 ; i<3 ; i++ ) {
			    elm_uindex[elm_dimcnt][i] = lindex[i]; } }
		  }
break;
case 68:
#line 654 "vs_y.y"
{
		    indcnt = 0;
		    lindex[indcnt] = yyvsp[0].ival;
		    lindex[1] = lindex[2] = -1;
		  }
break;
case 69:
#line 660 "vs_y.y"
{
		    if ( indcnt == 2 ) {
			yyerror ( "syntax error" );
			YYERROR;
		    }
		    else {
		    	indcnt++;
		    	lindex[indcnt] = yyvsp[0].ival;
		    }
		  }
break;
case 70:
#line 673 "vs_y.y"
{ yyval.ival = -1; }
break;
case 72:
#line 680 "vs_y.y"
{
		    return ( 0 );
		  }
break;
#line 1201 "vs_y.c"
    }
    yyssp -= yym;
    yystate = *yyssp;
    yyvsp -= yym;
    yym = yylhs[yyn];
    if (yystate == 0 && yym == 0)
    {
#if YYDEBUG
        if (yydebug)
            printf("%sdebug: after reduction, shifting from state 0 to\
 state %d\n", YYPREFIX, YYFINAL);
#endif
        yystate = YYFINAL;
        *++yyssp = YYFINAL;
        *++yyvsp = yyval;
        if (yychar < 0)
        {
            if ((yychar = yylex()) < 0) yychar = 0;
#if YYDEBUG
            if (yydebug)
            {
                yys = 0;
                if (yychar <= YYMAXTOKEN) yys = yyname[yychar];
                if (!yys) yys = "illegal-symbol";
                printf("%sdebug: state %d, reading %d (%s)\n",
                        YYPREFIX, YYFINAL, yychar, yys);
            }
#endif
        }
        if (yychar == 0) goto yyaccept;
        goto yyloop;
    }
    if ((yyn = yygindex[yym]) && (yyn += yystate) >= 0 &&
            yyn <= YYTABLESIZE && yycheck[yyn] == yystate)
        yystate = yytable[yyn];
    else
        yystate = yydgoto[yym];
#if YYDEBUG
    if (yydebug)
        printf("%sdebug: after reduction, shifting from state %d \
to state %d\n", YYPREFIX, *yyssp, yystate);
#endif
    if (yyssp >= yysslim && yygrowstack())
    {
        goto yyoverflow;
    }
    *++yyssp = yystate;
    *++yyvsp = yyval;
    goto yyloop;
yyoverflow:
    yyerror("yacc stack overflow");
yyabort:
    return (1);
yyaccept:
    return (0);
}
