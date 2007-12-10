/* bi_factor - prime-factor bigints
**
** Copyright (C) 1987,1990,1995,2000 by Jef Poskanzer <jef@mail.acme.com>.
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
#include <stdio.h>
#include <string.h>

#include "bigint.h"

#include "low_primes.h"

static char* argv0;


static void
print_fact( bigint fact, int power )
    {
    putchar( ' ' );
    bi_print( stdout, fact );

    if ( power != 1 )
        (void) printf( "^%d", power );
    }


static int
test_fact( bigint* numP, int fact )
    {
    int power;
    bigint t;

    power = 0;
    for (;;)
	{
	t = bi_int_divide( bi_copy( *numP ), fact );
	if ( bi_compare( bi_int_multiply( bi_copy( t ), fact ), bi_copy( *numP ) ) != 0 )
	    break;
	bi_free( *numP );
	*numP = t;
	++power;
	}

    if ( power != 0 )
        print_fact( int_to_bi( fact ), power );

    if ( bi_compare( t, int_to_bi( fact ) ) > 0 )
        return 1;

    return 0;
    }


static void
factor( bigint num )
    {
    int p;
    int fact;

    bi_print( stdout, bi_copy( num ) );
    (void) printf( " =" );

    if ( bi_is_zero( bi_copy( num ) ) || bi_is_one( bi_copy( num ) ) )
	print_fact( num, 1 );
    else
        {
	for ( p = 0; p < sizeof(low_primes)/sizeof(*low_primes); ++p )
	    if ( ! test_fact( &num, low_primes[p] ) )
		goto done;
	fact = ( low_primes[p - 1] + 5 ) / 6 * 6 - 1;
	for (;;)
	    {
	    if ( ! test_fact( &num, fact ) )
		break;
	    fact += 2;
	    if ( ! test_fact( &num, fact ) )
		break;
	    fact += 4;
	    }
	done:
        if ( bi_compare( bi_copy( num ), bi_1 ) != 0 )
            print_fact( bi_copy( num ), 1 );
	bi_free( num );
        }

    printf( "\n" );
    }


static void
parse_arg( char* arg )
    {
    int i;
    bigint n, n2;

    i = strspn( arg, "0123456789" );
    if ( i == strlen( arg ) )
	factor( str_to_bi( arg ) );
    else
	{
	if ( arg[i] == '-' )
	    {
	    n = str_to_bi( arg );
	    n2 = str_to_bi( &arg[i+1] );
	    for ( ; bi_compare( bi_copy( n ), bi_copy( n2 ) ) < 1;
		  n = bi_int_add( n, 1 ) )
		factor( bi_copy( n ) );
	    bi_free( n );
	    bi_free( n2 );
	    }
	else if ( arg[i] == '.' )
	    {
	    if ( strspn( &arg[i], "." ) != strlen( arg ) - i )
		{
		(void) fprintf(
		    stderr, "%s: wildcards must trail number\n", argv0 );
		exit( 1 );
		}
	    n = str_to_bi( arg );
	    switch ( strlen( arg ) - i )
		{
		case 1:
		n = bi_int_multiply( n, 10 );
		n2 = bi_int_add( bi_copy( n ), 9 );
		break;

		case 2:
		n = bi_int_multiply( n, 100 );
		n2 = bi_int_add( bi_copy( n ), 99 );
		break;

		case 3:
		n = bi_int_multiply( n, 1000 );
		n2 = bi_int_add( bi_copy( n ), 999 );
		break;

		default:
		(void) fprintf(
		    stderr, "%s: too many wildcard chars\n", argv0 );
		exit( 1 );
		}
	    for ( ; bi_compare( bi_copy( n ), bi_copy( n2 ) ) < 1;
		  n = bi_int_add( n, 1 ) )
		factor( bi_copy( n ) );
	    bi_free( n );
	    bi_free( n2 );
	    }
	else
	    {
	    (void) fprintf(
		stderr, "usage:  %s <int> <int>-<int> ...\n", argv0 );
	    exit( 1 );
	    }
	}
    }


int
main( int argc, char** argv )
    {
    int i;
    char buf[1000];

    argv0 = argv[0];
    bi_initialize();
    bi_no_check();

    if ( argc == 1 )
	/* No args, read numbers from stdin. */
        while ( fgets( buf, sizeof(buf), stdin ) != (char*) 0 )
	    parse_arg( buf );
    else
        for ( i = 1; i < argc; ++i )
            parse_arg( argv[i] );

    exit( 0 );
    }
