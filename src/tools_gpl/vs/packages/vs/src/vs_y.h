#ifndef YYERRCODE
#define YYERRCODE 256
#endif

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
typedef union {
    int    ival;
    float  fval;
    char * string;
    } YYSTYPE;
extern YYSTYPE yylval;
