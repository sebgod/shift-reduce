
#ifndef sr_GPARSE_H_
#define sr_GPARSE_H_

#ifdef __cplusplus
extern "C" {
#endif

//
// Action Types
//
#define ACTION_SHIFT    1
#define ACTION_REDUCE   2
#define ACTION_GOTO     3
#define ACTION_ACCEPT   4

//
// Symbol Types
//
#define SymbolTypeNonterminal		0
#define SymbolTypeTerminal			1
#define SymbolTypeWhitespace		2
#define SymbolTypeEnd End Character	3
#define SymbolTypeCommentStart		4
#define SymbolTypeCommentEnd		5
#define SymbolTypeCommentLine		6
#define SymbolTypeError				7

#define EEOF 512

//
// Initial Buffer Sizes - Paul Hulskamp
//
#define STACK_SIZE 64
#define TOKEN_SIZE 32
#define RT_BUFF_SIZE 128
#define RT_INDEX_SIZE 32

//
// Callback function prototypes
//

struct sr_parser;       // forward decl
struct sr_parse_input;  // foward decl

typedef char (*sr_cbmatchtoken)(struct sr_parser* parser,
                                void* user, short type, char* name, short symbol);

typedef void (*sr_cbneedinput)(struct sr_parse_input* pconfig);

typedef struct sr_symbol {
	char*	Name;
	short	Type;
} sr_symbol;

typedef struct sr_rule {
	short	NonTerminal;
	short*	symbol;
	int		nsymbol;
} sr_rule;

typedef struct sr_edge {
	short CharSetIndex;
	short TargetIndex;
} sr_edge;

typedef struct sr_dfa_state {
	char Accept;
	short AcceptIndex;
	sr_edge*	edge;
	short	nedge;
} sr_dfa_state;

typedef struct sr_action {
	short SymbolIndex;
	short Action;
	short Target;
} sr_action;

typedef struct sr_lalr_state {
	sr_action*	action;
	short		naction;
} sr_lalr_state;

typedef struct sr_token {
	short	id;
	char*	lexeme;
} sr_token;

typedef struct sr_stack_element {
	sr_symbol	symbol;
	sr_token	token;
	short		state;
	short		rule;
	void*		user_data;
	// Reduction Tree
	short*		rtchildren;
	short		nrtchild;
	short		rtchildofs;
	// End Reduction Tree
} sr_stack_element;

typedef struct sr_parse_input {
	struct sr_parser* parser;
	char*	buf;
	int		ncount;
	int		nofs;
	int		nbufsize;
	sr_cbneedinput cbneedinput;
	char	bpreserve;
	void*	user;
} sr_parse_input;

// parser configuration
typedef struct sr_parse_config {
	short			init_dfa;
	short			init_lalr;
	char			case_sensitive;
	short			start_symbol;
	const char**	charset;
	short			ncharset;
	sr_dfa_state*	dfa_state;
	short			ndfa_state;
	sr_symbol*	sym;
	short			nsym;
	sr_rule*	rule;
	short			nrule;
	sr_lalr_state*	lalr_state;
	short			nlalr_state;
} sr_parse_config;

// current parse info
typedef struct sr_parser {
	char					reduction;
	short					reduce_rule;
	char*					lexeme;
	int						nlexeme;
	short					symbol;
	short					lalr_state;
	void*					symbol_userdata;
	sr_stack_element*	    stack;
	short					nstack;
	short					nstacksize;
	size_t					nstackofs;
	sr_parse_config*	    pconfig;
	sr_cbmatchtoken			cbmatchtoken;
	void*					user_callback;
	sr_parse_input		    input;
	sr_token*	    		tokens;
	short					ntokens;

	// Reduction Tree
	sr_stack_element*	    rt;
	short					rtsize;
	int				    	rtofs;
	// End Reduction Tree

} sr_parser;

//
//	Parser functions
//
			// create from file
sr_parse_config*	parser_config_create_file(const char* filename);

			// create from memory
sr_parse_config*	parser_config_create_mem(char* buffer, int len);

			// delete
void		parser_config_delete(sr_parse_config* pconfig);


//
// Parser state functions
//

			// init the parser state
sr_parser*	parser_create(sr_parse_config* pconfig);

			// reset the parser state
void		parser_reset_state(sr_parser* parser);

			// delete the parser state
void		parser_delete(sr_parser* parser);

			// get a lexeme from the token stack
const char* parser_get_child_lexeme(sr_parser* parser, int index);
void*		parser_get_child_userdata(sr_parser* parser, int index);

			// set the current lexeme
void		parser_set_lexeme(sr_parser* parser, const char* lexeme, void* value);

			// get symbol info
char		parser_get_symbol(sr_parser* parser, int symbol, sr_symbol* psymbol);

			// get a pointer to the input structure
sr_parse_input* parser_get_input(sr_parser* parser);

			// get the last scanner lexeme
const char*	scanner_get_lexeme(sr_parser* parser);

void parser_set_userdata(sr_parser* parser, void* value);

void*	parser_get_userdata(sr_parser* parser);

//
// Scan/Parse functions
//
			// default scanner match function
char scanner_def_matchtoken(sr_parser* parser, void* user, short type, char* name, short symbol);

			// check for eof on the input stream
char scanner_get_eof(sr_parser* parser);

			// get the next character from the input stream
short scanner_get_char(sr_parser* parser);

			// increment to next character in input stream
void scanner_next_char(sr_parser* parser);

			// get the next token
short		scanner_scan(sr_parser* parser);

			// parse
short		parser_parse(sr_parser* parser);

//
// Reduction Tree functions
//
sr_stack_element* get_rt_entry(sr_parser* parser, short idx);

#ifdef __cplusplus
}
//
// C++ Wrapper class
//
class Parser {
public:
	Parser(const char* config_filename) {
		m_config = 0; m_parser = 0;
		m_config = parser_config_create_file(config_filename);
		if (m_config) m_parser = parser_create(m_config);
	}
	~Parser() {
		if (m_parser) parser_delete(m_parser);
		if (m_config) parser_config_delete(m_config);
		m_parser = 0; m_config = 0;
	}
	void reset() {parser_reset_state(m_parser);}
	char isopen() {return (m_config && m_parser)?1:0;}
	short parse() {return parser_parse(m_parser);} 
	const char* get_child_lexeme(int idx) {return parser_get_child_lexeme(m_parser,idx);}
	void* get_child_userdata(int idx) {return parser_get_child_userdata(m_parser,idx);}

	void* get_userdata() {return parser_get_userdata(m_parser);}
	void set_userdata(void* value) {parser_set_userdata(m_parser,value);}
	
	sr_stack_element* get_rt_element(short idx){
#ifdef sr_DEBUG
	void display_rt(){ print_reduction_tree(m_parser);}
#endif // sr_DEBUG

	sr_parse_config*	m_config;
	sr_parser*		m_parser;
};
#endif

#endif
