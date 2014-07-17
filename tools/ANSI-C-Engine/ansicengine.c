#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <string.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "parser.h"

//
// This function is called when the scanner matches a token
//
// Our implementation handles the "Comment Start" symbol, and scans through
// the input until the token closure is found
//
// All other tokens are refered to the default match function
//
char cbmatchtoken(sr_parser* parser, void* user, short type, char* name, short symbol)
{
    short c;
    switch(type) {
        case SymbolTypeCommentStart:
            {
                while((c = scanner_get_char(parser)) != EEOF) {
                    if (c == '*') {
                        scanner_next_char(parser);
                        c = scanner_get_char(parser);
                        if (c != EEOF) {
                            if (c == '/') {
                                scanner_next_char(parser);
                                return 0;
                            }
                        }
                    }
                    scanner_next_char(parser);
                }
                parser->symbol = 0; // eof
                return 0;
            }
            
        default:
                return scanner_def_matchtoken(parser,user,type,name,symbol);
    }
}

//
// This function is called when the scanner needs more data
//
// If the pinput->bpreserve flag is set(because the scanner may need
// to backtrack), then the data in the current input buffer must be
// preserved.  This is done by increasing the buffer size and copying
// the old data to the new buffer.
// If the pinput->bpreserve flag is not set, the new data can be copied
// over the existing buffer.
//
// If the input buffer is empty after this callback returns(because no
// more data was added), the scanner function will return either:
//   1] the last accepted token
//   2] an eof, if no token has been accepted yet
//
void cbgetinput(sr_parse_input* pinput)
{
    int nr;
    if (!feof((FILE*)pinput->user)) {
        if (pinput->bpreserve) {
            // copy the existing buffer to a new, larger one
            char* p = (char*)malloc(2048 + pinput->ncount);
            pinput->nbufsize += 2048;
            memcpy(p, pinput->buf, pinput->ncount);
            free(pinput->buf);
            pinput->buf = p;
            nr = (int)fread(pinput->buf, 1, 2048, (FILE*)pinput->user);
            pinput->ncount += nr; // increase the character count
        } else {
            nr = (int)fread(pinput->buf, 1, pinput->nbufsize, (FILE*)pinput->user);
            pinput->nofs = 0;    // reset the offset
            pinput->ncount = nr; // set the character count
        }
    }
}

void usage()
{
    printf("usage: ansicengine.exe <.cgt file> <input file>\n");
}

void print_indent(int indent)
{
    int i;
    for(i=0;i<indent;++i)printf("| ");
}

void print_stack_element(sr_stack_element* se)
{
    printf("+-<%s> ::= %s\n", se->symbol.Name, se->token.lexeme);
}

void print_rt_tree(sr_parser* parser, int rtpos, int indent)
{
    int j;
    print_indent(indent);
    print_stack_element(parser->rt+rtpos);
    for(j=parser->rt[rtpos].nrtchild-1;j>=0;--j){
        if (parser->rt[parser->rt[rtpos].rtchildren[j]].symbol.Type != 1) // non terminal
            print_rt_tree(parser, parser->rt[rtpos].rtchildren[j], indent+1);
        else {
            print_indent(indent);
            print_stack_element(parser->rt+(parser->rt[rtpos].rtchildren[j]));
        }
    }
}

// Accepts 2 arguments <.cgt file> <input file>
int main(int argc, char *argv[])
{
    int     run = 1;
    FILE*   fin = 0;
    sr_parser* parser = 0;
    sr_parse_config* parser_config = 0;
    if (argc < 3){
        usage();
        return EXIT_FAILURE;
    }
    // Open .cgt file
    parser_config = parser_config_create_file(argv[1]);
    if (!parser_config){
        printf("Could not open cgt file %s\n", argv[1]);
        return EXIT_FAILURE;
    }
    parser = parser_create(parser_config);
    if (!parser){
        printf("Error creating parser\n");
        return EXIT_FAILURE;
    }

    // Open input file
    fin = fopen(argv[2], "rb");
    if (!fin) {
        printf("Could not open input file %s\n", argv[1]);
        return EXIT_FAILURE;
    }

    // set the input
    parser->input.buf = (char*)malloc(2048);
    parser->input.nbufsize = 2048;
    parser->input.cbneedinput = cbgetinput;
    parser->input.user = fin;

    // set match callback
    parser->cbmatchtoken = cbmatchtoken;

    
    while(run)
    {
        short p = parser_parse(parser);
        if (p < 0){
            printf("Error parsing\n");
            return EXIT_FAILURE;
        } else if(0 == p){
            run = 0;
        }
    }

    // Print parse tree
    print_rt_tree(parser, 0, 0);

    if (parser) parser_delete(parser);
    if (parser_config) parser_config_delete(parser_config);

    return EXIT_SUCCESS;
}
