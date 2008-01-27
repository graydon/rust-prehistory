%{
/* bi_bc_parse.y - parser for bigint version of bc
**
** Copyright © 2000 by Jef Poskanzer <jef@mail.acme.com>.
** All rights reserved.
**
** Redistribution and use in source and binary forms, with or without
** modification, are permitted provided that the following conditions
** are met:
** 1. Redistributions of source code must retain the above copyright
**    notice, this list of conditions and the following disclaimer.
** 2. Redistributions in binary form must reproduce the above copyright
**    notice, this list of conditions and the following disclaimer in the
**    documentation and/or other materials provided with the distribution.
**
** THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
** ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
** IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
** ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
** FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
** DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
** OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
** HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
** LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
** OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
** SUCH DAMAGE.
*/

#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <time.h>
#include <setjmp.h>
#include <signal.h>

#include "bigint.h"

#define YYSTYPE bigint

static int yyparse();
extern int yylex();

bigint prev_val;

static bigint vars[26];

static jmp_buf jb;


static void
init( void )
    {
    int i;

    bi_initialize();
    bi_no_check();
    prev_val = bi_0;
    for ( i = 0; i < 26; ++i )
	vars[i] = bi_0;
    }


static void
term( void )
    {
    int i;

    bi_free( prev_val );
    for ( i = 0; i < 26; ++i )
	bi_free( vars[i] );
    bi_terminate();
    }


void
yyerror( char* msg )
    {
    (void) fprintf( stderr, "%s\n", msg );
    }


static void
handle_sig( int sig )
    {
    (void) fprintf( stderr, "Floating point exception (caught)\n" );
    longjmp( jb, 1 );
    }


static FILE*
save_vars( void )
    {
    FILE* fp;
    int i;

    fp = tmpfile();
    for ( i = 0; i < 26; ++i )
	{
	bi_print( fp, vars[i] );
	(void) fputc( '\n', fp );
	}
    return fp;
    }


static void
restore_vars( FILE* fp )
    {
    int i;

    rewind( fp );
    for ( i = 0; i < 26; ++i )
	vars[i] = bi_scan( fp );
    (void) fclose( fp );
    }


static void
reinit( void )
    {
    FILE* fp;

    fp = save_vars();
    init();
    restore_vars( fp );
    }


int
main( int argc, char** argv )
    {
    srandom( (int) time( (time_t*) 0 ) ^ getpid() );

    signal( SIGFPE, handle_sig );

    init();

    if ( setjmp( jb ) != 0 )
	/* Re-init to avoid whining about lost bigints. */
	reinit();

    yyparse();

    term();
    exit( 0 );
    }

%}

%token NUMBER
%token SQRT
%token GCD
%token LCM
%token MODPOW
%token MODINV
%token RANDOM
%token JACOBI
%token ISPRIME
%token GENPRIME
%token BITS
%token BINDIV
%token VAR

%left '+' '-'
%left '*' '/' '%'
%right '^'
%left '!'
%left NEG

%%

input			: /* empty */
			| input line
			;
line			: '\n'
			| expr '\n'
			    {
			    bi_free( prev_val );
			    prev_val = bi_copy( $1 );
			    bi_print( stdout, $1 );
			    putchar( '\n' );
			    }
			| VAR '=' expr '\n'
			    {
			    int i;
			    i = bi_to_int( $1 );
			    bi_free( vars[i] );
			    vars[i] = $3;
			    }
			| error '\n'
			    {
			    /* Re-init to avoid whining about lost bigints. */
			    reinit();
			    yyerrok;
			    }
			;
expr			: expr '+' expr
			    { $$ = bi_add( $1, $3 ); }
			| expr '-' expr
			    { $$ = bi_subtract( $1, $3 ); }
			| '-' expr  %prec NEG
			    { $$ = bi_negate( $2 ); }
			| expr '*' expr
			    { $$ = bi_multiply( $1, $3 ); }
			| expr '/' expr
			    { $$ = bi_divide( $1, $3 ); }
			| expr '%' expr
			    { $$ = bi_rem( $1, $3 ); }
			| expr '^' expr
			    { $$ = bi_power( $1, $3 ); }
			| expr '!'
			    { $$ = bi_factorial( $1 ); }
			| SQRT '(' expr ')'
			    { $$ = bi_sqrt( $3 ); }
			| GCD '(' expr ',' expr ')'
			    { $$ = bi_gcd( $3, $5 ); }
			| LCM '(' expr ',' expr ')'
			    { $$ = bi_lcm( $3, $5 ); }
			| MODPOW '(' expr ',' expr ',' expr ')'
			    { $$ = bi_mod_power( $3, $5, $7 ); }
			| MODINV '(' expr ',' expr ')'
			    { $$ = bi_mod_inverse( $3, $5 ); }
			| RANDOM '(' expr ')'
			    { $$ = bi_random( $3 ); }
			| JACOBI '(' expr ',' expr ')'
			    { $$ = bi_jacobi( $3, $5 ); }
			| ISPRIME '(' expr ',' expr ')'
			    { $$ = int_to_bi( bi_is_probable_prime( $3, bi_to_int( $5 ) ) ); }
			| GENPRIME '(' expr ',' expr ')'
			    { $$ = bi_generate_prime( bi_to_int( $3 ), bi_to_int( $5 ) ); }
			| BITS '(' expr ')'
			    { $$ = int_to_bi( bi_bits( $3 ) ); }
			| BINDIV '(' expr ',' expr ')'
			    { $$ = bi_binary_divide( $3, $5 ); }
			| '(' expr ')'
			    { $$ = $2; }
			| NUMBER
			    { $$ = $1; }
			| VAR
			    { $$ = bi_copy( vars[bi_to_int( $1 )] ); }
			;
%%
