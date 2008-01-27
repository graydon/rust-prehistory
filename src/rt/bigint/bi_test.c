/* bi_test - some simple tests for the bigint package
**
** Copyright (C) 2000 by Jef Poskanzer <jef@mail.acme.com>.
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


int
main( int argc, char** argv )
    {
    int i1, i2, i3, i4, i5, imod;
    bigint bi1, bi2, bi3, bi4, bi5;
    bigint biprod1a, biprod2a, biprod3a, biprod4a;
    bigint biprod1b, biprod2b, biprod3b, biprod4b;
    bigint bidiv1a, bidiv2a, bidiv3a, bidiv4a;
    bigint bidiv1b, bidiv2b, bidiv3b, bidiv4b;
    bigint bisqr1, bisqrt1, bisqr2, bisqrt2, bisqr3, bisqrt3;
    bigint bibigsqr, bibigsqrt, bibigdiv;

    bi_initialize();

    i1 = 999999001L;
    bi1 = int_to_bi( i1 );
    (void) printf( "%d  =  ", i1 );
    bi_print( stdout, bi_copy( bi1 ) );
    (void) printf( "\n" );

    i2 = 999999229L;
    bi2 = int_to_bi( i2 );
    (void) printf( "%d  =  ", i2 );
    bi_print( stdout, bi_copy( bi2 ) );
    (void) printf( "\n" );

    i3 = 999999503L;
    bi3 = int_to_bi( i3 );
    (void) printf( "%d  =  ", i3 );
    bi_print( stdout, bi_copy( bi3 ) );
    (void) printf( "\n" );

    i4 = 999999751L;
    bi4 = int_to_bi( i4 );
    (void) printf( "%d  =  ", i4 );
    bi_print( stdout, bi_copy( bi4 ) );
    (void) printf( "\n" );

    i5 = 999999937L;
    bi5 = int_to_bi( i5 );
    (void) printf( "%d  =  ", i5 );
    bi_print( stdout, bi_copy( bi5 ) );
    (void) printf( "\n" );

    fflush( stdout );

    biprod1a = bi_int_multiply( bi_copy( bi1 ), i2 );
    biprod1b = bi_multiply( bi_copy( bi1 ), bi_copy( bi2 ) );
    (void) printf( "%d  *  %d  =  ", i1, i2 );
    bi_print( stdout, bi_copy( biprod1a ) );
    (void) printf( "\n" );
    if ( bi_compare( bi_copy( biprod1a ), bi_copy( biprod1b ) ) != 0 )
	{
	(void) printf( "ERROR multiply " );
	bi_print( stdout, bi_copy( biprod1b ) );
	(void) printf( "\n" );
	}
    biprod2a = bi_int_multiply( bi_copy( biprod1a ), i3 );
    biprod2b = bi_multiply( bi_copy( biprod1b ), bi_copy( bi3 ) );
    bi_print( stdout, bi_copy( biprod1a ) );
    (void) printf( "  *  %d  =  ", i3 );
    bi_print( stdout, bi_copy( biprod2a ) );
    (void) printf( "\n" );
    if ( bi_compare( bi_copy( biprod2a ), bi_copy( biprod2b ) ) != 0 )
	{
	(void) printf( "ERROR multiply " );
	bi_print( stdout, bi_copy( biprod2b ) );
	(void) printf( "\n" );
	}
    biprod3a = bi_int_multiply( bi_copy( biprod2a ), i4 );
    biprod3b = bi_multiply( bi_copy( biprod2b ), bi_copy( bi4 ) );
    bi_print( stdout, bi_copy( biprod2a ) );
    (void) printf( "  *  %d  =  ", i4 );
    bi_print( stdout, bi_copy( biprod3a ) );
    (void) printf( "\n" );
    if ( bi_compare( bi_copy( biprod3a ), bi_copy( biprod3b ) ) != 0 )
	{
	(void) printf( "ERROR multiply " );
	bi_print( stdout, bi_copy( biprod3b ) );
	(void) printf( "\n" );
	}
    biprod4a = bi_int_multiply( bi_copy( biprod3a ), i5 );
    biprod4b = bi_multiply( bi_copy( biprod3b ), bi_copy( bi5 ) );
    bi_print( stdout, bi_copy( biprod3a ) );
    (void) printf( "  *  %d  =  ", i5 );
    bi_print( stdout, bi_copy( biprod4a ) );
    (void) printf( "\n" );
    if ( bi_compare( bi_copy( biprod4a ), bi_copy( biprod4b ) ) != 0 )
	{
	(void) printf( "ERROR multiply " );
	bi_print( stdout, bi_copy( biprod4b ) );
	(void) printf( "\n" );
	}

    fflush( stdout );

    bidiv1a = bi_int_divide( bi_copy( biprod4a ), i1 );
    bidiv1b = bi_divide( bi_copy( biprod4a ), bi_copy( bi1 ) );
    bi_print( stdout, bi_copy( biprod4a ) );
    (void) printf( "  /  %d  =  ", i1 );
    bi_print( stdout, bi_copy( bidiv1a ) );
    (void) printf( "\n" );
    if ( bi_compare( bi_copy( bidiv1a ), bi_copy( bidiv1b ) ) != 0 )
	{
	(void) printf( "ERROR divide " );
	bi_print( stdout, bi_copy( bidiv1b ) );
	(void) printf( "\n" );
	}
    bidiv2a = bi_int_divide( bi_copy( bidiv1a ), i2 );
    bidiv2b = bi_divide( bi_copy( bidiv1b ), bi_copy( bi2 ) );
    bi_print( stdout, bi_copy( bidiv1a ) );
    (void) printf( "  /  %d  =  ", i2 );
    bi_print( stdout, bi_copy( bidiv2a ) );
    (void) printf( "\n" );
    if ( bi_compare( bi_copy( bidiv2a ), bi_copy( bidiv2b ) ) != 0 )
	{
	(void) printf( "ERROR divide " );
	bi_print( stdout, bi_copy( bidiv2b ) );
	(void) printf( "\n" );
	}
    bidiv3a = bi_int_divide( bi_copy( bidiv2a ), i3 );
    bidiv3b = bi_divide( bi_copy( bidiv2b ), bi_copy( bi3 ) );
    bi_print( stdout, bi_copy( bidiv2a ) );
    (void) printf( "  /  %d  =  ", i3 );
    bi_print( stdout, bi_copy( bidiv3a ) );
    (void) printf( "\n" );
    if ( bi_compare( bi_copy( bidiv3a ), bi_copy( bidiv3b ) ) != 0 )
	{
	(void) printf( "ERROR divide " );
	bi_print( stdout, bi_copy( bidiv3b ) );
	(void) printf( "\n" );
	}
    bidiv4a = bi_int_divide( bi_copy( bidiv3a ), i4 );
    bidiv4b = bi_divide( bi_copy( bidiv3b ), bi_copy( bi4 ) );
    bi_print( stdout, bi_copy( bidiv3a ) );
    (void) printf( "  /  %d  =  ", i4 );
    bi_print( stdout, bi_copy( bidiv4a ) );
    (void) printf( "\n" );
    if ( bi_compare( bi_copy( bidiv4a ), bi_copy( bidiv4b ) ) != 0 )
	{
	(void) printf( "ERROR divide " );
	bi_print( stdout, bi_copy( bidiv4b ) );
	(void) printf( "\n" );
	}
    if ( bi_compare( bi_copy( bidiv4a ), bi_copy( bi5 ) ) != 0 )
	(void) printf( "ERROR int divides\n" );

    fflush( stdout );

    imod = bi_int_mod( bi_copy( biprod4a ), i1 );
    bi_print( stdout, bi_copy( biprod4a ) );
    (void) printf( "  %%  %d  =  %d\n", i1, imod );
    if ( imod != 0 )
	(void) printf( "ERROR int mod\n" );
    imod = bi_int_mod( bi_copy( biprod4a ), i2 );
    bi_print( stdout, bi_copy( biprod4a ) );
    (void) printf( "  %%  %d  =  %d\n", i2, imod );
    if ( imod != 0 )
	(void) printf( "ERROR int mod\n" );
    imod = bi_int_mod( bi_copy( biprod4a ), i3 );
    bi_print( stdout, bi_copy( biprod4a ) );
    (void) printf( "  %%  %d  =  %d\n", i3, imod );
    if ( imod != 0 )
	(void) printf( "ERROR int mod\n" );
    imod = bi_int_mod( bi_copy( biprod4a ), i4 );
    bi_print( stdout, bi_copy( biprod4a ) );
    (void) printf( "  %%  %d  =  %d\n", i4, imod );
    if ( imod != 0 )
	(void) printf( "ERROR int mod\n" );
    imod = bi_int_mod( bi_copy( biprod4a ), i5 );
    bi_print( stdout, bi_copy( biprod4a ) );
    (void) printf( "  %%  %d  =  %d\n", i5, imod );
    if ( imod != 0 )
	(void) printf( "ERROR int mod\n" );

    fflush( stdout );

    bisqr1 = bi_multiply( bi_copy( bi1 ), bi_copy( bi1 ) );
    bisqrt1 = bi_sqrt( bi_copy( bisqr1 ) );
    bi_print( stdout, bi_copy( bi1 ) );
    printf( " ^ 2  =  " );
    bi_print( stdout, bi_copy( bisqr1 ) );
    printf( "\n" );
    printf( "sqrt( " );
    bi_print( stdout, bi_copy( bisqr1 ) );
    printf( " )  =  " );
    bi_print( stdout, bi_copy( bisqrt1 ) );
    printf( "\n" );
    if ( bi_compare( bi_copy( bisqrt1 ), bi_copy( bi1 ) ) != 0 )
	(void) printf( "ERROR sqrt1\n" );

    fflush( stdout );

    bisqr2 = bi_add( bi_multiply( bi_copy( bi1 ), bi_copy( bi1 ) ), bi_half( bi_copy( bi1 ) ) );
    bisqrt2 = bi_sqrt( bi_copy( bisqr2 ) );
    bi_print( stdout, bi_copy( bi1 ) );
    printf( " ^ 2 + " );
    bi_print( stdout, bi_half( bi_copy( bi1 ) ) );
    printf( "  =  " );
    bi_print( stdout, bi_copy( bisqr2 ) );
    printf( "\n" );
    printf( "sqrt( " );
    bi_print( stdout, bi_copy( bisqr2 ) );
    printf( " )  =  " );
    bi_print( stdout, bi_copy( bisqrt2 ) );
    printf( "\n" );
    if ( bi_compare( bi_copy( bisqrt2 ), bi_copy( bi1 ) ) != 0 )
	(void) printf( "ERROR sqrt2\n" );

    fflush( stdout );

    /* x^2 + 2x == (x+1)^2 - 1 */
    bisqr3 = bi_add( bi_multiply( bi_copy( bi1 ), bi_copy( bi1 ) ), bi_double( bi_copy( bi1 ) ) );
    bisqrt3 = bi_sqrt( bi_copy( bisqr3 ) );
    bi_print( stdout, bi_copy( bi1 ) );
    printf( " ^ 2 + " );
    bi_print( stdout, bi_double( bi_copy( bi1 ) ) );
    printf( "  =  " );
    bi_print( stdout, bi_copy( bisqr3 ) );
    printf( "\n" );
    printf( "sqrt( " );
    bi_print( stdout, bi_copy( bisqr3 ) );
    printf( " )  =  " );
    bi_print( stdout, bi_copy( bisqrt3 ) );
    printf( "\n" );
    if ( bi_compare( bi_copy( bisqrt3 ), bi_copy( bi1 ) ) != 0 )
	(void) printf( "ERROR sqrt3\n" );

    fflush( stdout );

    bibigsqr = bi_multiply( bi_copy( biprod4a ), bi_copy( biprod4a ) );
    bi_print( stdout, bi_copy( biprod4a ) );
    printf( " * " );
    bi_print( stdout, bi_copy( biprod4a ) );
    printf( "  =  " );
    bi_print( stdout, bi_copy( bibigsqr ) );
    printf( "\n" );
    bibigdiv = bi_divide( bi_copy( bibigsqr ), bi_copy( biprod4a ) );
    bi_print( stdout, bi_copy( bibigsqr ) );
    printf( " / " );
    bi_print( stdout, bi_copy( biprod4a ) );
    printf( "  =  " );
    bi_print( stdout, bi_copy( bibigdiv ) );
    printf( "\n" );
    if ( bi_compare( bi_copy( bibigdiv ), bi_copy( biprod4a ) ) != 0 )
	(void) printf( "ERROR big divide\n" );
    bibigsqrt = bi_sqrt( bi_copy( bibigsqr ) );
    printf( "sqrt( " );
    bi_print( stdout, bi_copy( bibigsqr ) );
    printf( " )  =  " );
    bi_print( stdout, bi_copy( bibigsqrt ) );
    printf( "\n" );
    if ( bi_compare( bi_copy( bibigsqrt ), bi_copy( biprod4a ) ) != 0 )
	(void) printf( "ERROR big sqrt\n" );

    bi_free( bi1 );
    bi_free( bi2 );
    bi_free( bi3 );
    bi_free( bi4 );
    bi_free( bi5 );
    bi_free( biprod1a );
    bi_free( biprod2a );
    bi_free( biprod3a );
    bi_free( biprod4a );
    bi_free( biprod1b );
    bi_free( biprod2b );
    bi_free( biprod3b );
    bi_free( biprod4b );
    bi_free( bidiv1a );
    bi_free( bidiv2a );
    bi_free( bidiv3a );
    bi_free( bidiv4a );
    bi_free( bidiv1b );
    bi_free( bidiv2b );
    bi_free( bidiv3b );
    bi_free( bidiv4b );
    bi_free( bisqr1 );
    bi_free( bisqrt1 );
    bi_free( bisqr2 );
    bi_free( bisqrt2 );
    bi_free( bisqr3 );
    bi_free( bisqrt3 );
    bi_free( bibigsqr );
    bi_free( bibigsqrt );
    bi_free( bibigdiv );

    bi_terminate();

    exit( 0 );
    }
