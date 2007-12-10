/* bi_bc_scan - scanner for bigint version of bc
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

#include <stdio.h>
#include <ctype.h>
#include <string.h>

#include "bigint.h"

#define YYSTYPE bigint
#include "bi_bc_parse.h"

extern bigint prev_val;

int
yylex( void )
    {
    int c, len, i;
    char buf[10000];

    /* Skip whitespace. */
    do {
	c = getchar();
	}
    while ( c == ' ' || c == '\t' );

    if ( isdigit( c ) )
	{
	buf[0] = c;
	len = 1;
	while ( len < sizeof(buf) )
	    {
	    c = getchar();
	    if ( ! isdigit( c ) )
		{
                ungetc( c, stdin );
                break;
                }
            buf[len++] = c;
            }
        buf[len] = '\0';
        yylval = str_to_bi( buf );
        return NUMBER;
        }
    else if ( isalpha( c ) )
	{
	buf[0] = c;
	len = 1;
	while ( len < sizeof(buf) )
	    {
	    c = getchar();
	    if ( ! isalpha( c ) )
		{
                ungetc( c, stdin );
                break;
                }
            buf[len++] = c;
            }
        buf[len] = '\0';
	for ( i = 0; i < len; ++i )
	    if ( isupper( buf[i] ) )
		buf[i] = tolower( buf[i] );
        if ( strcmp( buf, "sqrt" ) == 0 )
	    return SQRT;
        else if ( strcmp( buf, "gcd" ) == 0 )
	    return GCD;
        else if ( strcmp( buf, "lcm" ) == 0 )
	    return LCM;
        else if ( strcmp( buf, "modpow" ) == 0 )
	    return MODPOW;
        else if ( strcmp( buf, "modinv" ) == 0 )
	    return MODINV;
        else if ( strcmp( buf, "random" ) == 0 )
	    return RANDOM;
        else if ( strcmp( buf, "jacobi" ) == 0 )
	    return JACOBI;
        else if ( strcmp( buf, "isprime" ) == 0 )
	    return ISPRIME;
        else if ( strcmp( buf, "genprime" ) == 0 )
	    return GENPRIME;
        else if ( strcmp( buf, "bits" ) == 0 )
	    return BITS;
        else if ( strcmp( buf, "bindiv" ) == 0 )
	    return BINDIV;
	else if ( len == 1 )
	    {
	    yylval = int_to_bi( buf[0] - 'a' );
	    return VAR;
	    }
	else
	    return '.';	/* junk */
        }
    else if ( c == '.' )
	{
	yylval = bi_copy( prev_val );
	return NUMBER;
	}
    else if ( c == EOF )
	return 0;
    else
	return c;
    }
