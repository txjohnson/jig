/*/// ==================================================================================================================================
 * Copyright (c) 2012 by Theodore Johnson
 * All rights reserved.
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the 'Software'),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, provided that the above copyright notice(s) and this
 * permission notice appear in all copies of the Software.  Acknowledgement
 * of the use of this Software in supporting documentation would be
 * appreciated but is not required.
 * 
 * THE SOFTWARE IS PROVIDED 'AS IS'.  USE ENTIRELY AT YOUR OWN RISK.
 * 
/*/// ----------------------------------------------------------------------------------------------------------------------------------
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <map>
#include "jig.h"
using namespace jig;
using namespace std;


/*/// ==================================================================================================================================
	Jig's version of Piurmarta's version of BASIC.
	Like Piurmarta's, this is a syntax directed interpreter.
	
	Within is are examples of:
		Specifying actions using lambdas
		Defining a custom input source.
		Using actions to store information for later use
	
/*/// ----------------------------------------------------------------------------------------------------------------------------------
namespace V {
	const int line        = 1;
	const int statement   = 2;
	const int exprlist    = 3;
	const int varlist     = 4;
	const int expression  = 5;
	const int term        = 6;
	const int factor      = 7;
	const int var         = 8;
	const int number      = 9;
	const int digit       = 10;
	const int string      = 11;
	const int relop       = 12;
	const int EQUAL       = 13;
	const int CLOSE       = 14;
	const int OPEN        = 15;
	const int SLASH       = 16;
	const int STAR        = 17;
	const int MINUS       = 18;
	const int PLUS        = 19;
	const int COMMA       = 20;
	const int SPACES      = 21;
	const int CR          = 22;
};

jig::markbegin markb;
jig::markend   marke;
jig::any       wildcard;

#define DO [](regs_t& yy, bool& yythunk)->void

/*/// ==================================================================================================================================
/*/// ----------------------------------------------------------------------------------------------------------------------------------
typedef int (*binop)(int, int);

typedef map <size_t, string> lines_t;

lines_t  lines;
int      variables [26];
vector<size_t> stack;

bool     batch = false;
size_t   pc = 0;
size_t   gopc = 0;

struct userstate {
};

int      num;
int      eleft;
int      fleft;
int      ileft;
int      right;
int      var;
binop    op;
string*  code;
bool     inprog = false;

int input (void);
bool set_code_from_line (size_t l);
bool gonextline ();
size_t find_nextline (size_t);
void load (const string&);
void save (const string&);
void type (const string&);
void accept (size_t, string&);

const char help[] =
"	print {num} | {string} [, {num} | {string} ...] [,]\n\
	if {expr} < | <= | <> | = | >= | > {expr} then {stmt}\n\
	input {var} [, {var} ...]\n\
	let {var} = {expr}\n\
	goto {expr}\n\
	gosub {expr}\n\
	end\n\
	return\n\
	list\n\
	clear\n\
	run [\"filename\"]\n\
	rem {comment...}\n\
	dir\n\
	type \"filename\"\n\
	save \"filename\"\n\
	load \"filename\"\n\
	bye | quit | exit\n\
	help\n\
";

int lessThan     (int lhs, int rhs)    { return lhs < rhs; }
int lessEqual    (int lhs, int rhs)    { return lhs <= rhs; }
int notEqual     (int lhs, int rhs)    { return lhs != rhs; }
int equalTo      (int lhs, int rhs)    { return lhs == rhs; }
int greaterEqual (int lhs, int rhs)    { return lhs >= rhs; }
int greaterThan  (int lhs, int rhs)    { return lhs > rhs; }

/*/// ==================================================================================================================================
/*/// ----------------------------------------------------------------------------------------------------------------------------------
string unused;
struct stringInput {
	typedef char value_t;
	
	stringInput () : str (&unused) { }
	~stringInput () { }
	
	value_t value() const { if (pos >= str->size()) return 0; else return (*str)[pos]; }
	bool advance () { if (pos >= str->size()) return false; ++pos; return true; }
	size_t  tell () { return pos; }
	void seek (size_t where) { pos = where; }
	
	void set_line (const string& s) { str = &s; pos = 0; }
	bool eof ()     { return pos >= str->size(); }
	
	const string* str;
	size_t pos = 0;
};

stringInput codestream;

/*/// ==================================================================================================================================
	
/*/// ----------------------------------------------------------------------------------------------------------------------------------
typedef jig::grammar<stringInput, userstate> basicGrammar;
typedef basicGrammar::regs_t  regs_t;

basicGrammar* create_grammar () {
	basicGrammar* g = new basicGrammar ();
	
	g->add (V::line, alt (
				  seq (V::SPACES, V::statement, V::CR)
				, seq (V::SPACES, V::number, markb, many(0,0, seq (ntst(V::CR), wildcard)), V::CR, marke, DO { accept (num, yy.text); })
				, seq (V::SPACES, V::CR)
				, seq (markb, many(0,0, seq (ntst(V::CR), wildcard)), V::CR, marke, DO { cout << "Syntax error at " << pc << "\n"; inprog = false; })
//				, seq (ntst (wildcard), DO { exit (0); })
	));

	g->add (V::statement, alt (
				  seq ("print", V::SPACES, V::exprlist)
				, seq ("if", V::SPACES, V::expression,            DO { ::ileft = num; },
						V::relop, V::expression,                  DO { if (!op (::ileft, num)) yythunk = true; },
						"then", V::SPACES, V::statement)
				, seq ("goto", V::SPACES, V::expression,          DO {
																		gopc = num;
																		if (!set_code_from_line (gopc)) {
																			cout << "Error at " << pc << ": no such line.\n"; inprog = false;
																		}
																	})
				, seq ("input", V::SPACES, V::varlist)
				, seq ("let", V::SPACES, V::var,                  DO { var = num; },
						V::EQUAL, V::expression,                  DO { variables[var] = num; })
				, seq ("gosub", V::SPACES, V::expression,         DO {
																		gopc = num;
																		::stack.push_back (pc);
																		if (!set_code_from_line (gopc)) {
																			cout << "Error at " << pc << ": no such line.\n"; inprog = false;
																		}
																	})
				, seq ("return", V::SPACES,                       DO {
																	if (stack.empty()) {
																		cout << "Error at " << pc << ": no gosub.\n";
																		inprog = false;
																	}
																	else {
																		pc = ::stack.back();
																		stack.pop_back();
																		gonextline();
																		gopc = pc;
																	}})
				, seq ("clear", V::SPACES,                        DO { lines.clear(); })
				, seq ("list", V::SPACES,                         DO {
																		for (auto i = lines.begin(); i != lines.end(); ++i)
																			cout << (*i).first << " " << (*i).second;
																	})
				, seq ("run", V::SPACES, V::string,               DO { load (yy.text); pc = 0; set_code_from_line (pc); inprog = true; })
				, seq ("run", V::SPACES,                          DO { pc = 0; set_code_from_line (pc); inprog = true; })
				, seq ("end", V::SPACES,                          DO { inprog = false; })
				, seq ("rem", V::SPACES, many(0,0, seq (ntst(V::CR), wildcard)))
				, seq (alt ("bye", "quit", "exit"), V::SPACES,    DO { exit (0); })
				, seq ("save", V::SPACES, V::string,              DO { save (yy.text); })
				, seq ("load", V::SPACES, V::string,              DO { load (yy.text); })
				, seq ("type", V::SPACES, V::string,              DO { type (yy.text); })
				, seq ("dir", V::SPACES,                          DO { system ("ls *.bas"); })
				, seq ("help", V::SPACES,                         DO { cerr << ::help; })
	));


	g->add (V::exprlist, seq(
				opt (alt (seq(V::string,                          DO { cout << yy.text.c_str(); }),
					seq(V::expression,                            DO { cout << num; }))),
				many (0,0, seq (V::COMMA, alt(seq(V::string,      DO { cout << yy.text.c_str(); }),
										seq(V::expression,        DO { cout << num; })))),
				alt (V::COMMA, seq (ntst(V::COMMA),               DO { cout << endl; }))
	));
	
	g->add (V::varlist, seq(V::var,                                   DO { variables[num] = input(); },
				many(0,0, seq(V::COMMA, V::var,                       DO { variables[num] = input(); }))));
	
	g->add (V::expression, seq(
				alt (seq(opt(V::PLUS), V::term,                       DO { ::eleft = num; }),
					seq(V::MINUS, V::term,                            DO { ::eleft = -num; })),
				many (0,0, alt (seq(V::PLUS, V::term,                 DO { ::eleft += num; }),
					seq(V::MINUS, V::term,                            DO { ::eleft -= num; }))),
					                                                  DO { num = ::eleft; }
	));
	
	g->add (V::term, seq(V::factor,                   DO { ::fleft = num; },
				many(0,0, alt(seq(V::STAR, V::factor, DO { ::fleft *= num; })
					, seq(V::SLASH, V::factor,        DO { ::fleft /= num; }))), DO { num = ::fleft; }
	));

	g->add (V::factor, alt(
				  seq(V::var,                                                   DO { num = variables[num]; })
				, V::number, seq (V::OPEN, V::expression, V::CLOSE)
	));

	g->add (V::var, seq(markb, rge('a', 'z'), marke, V::SPACES,                 DO { num = yy.text[0] - 'a'; }
	));
	
	g->add (V::number, seq (markb, many(1,0, V::digit), marke, V::SPACES,       DO { num = atoi(yy.text.c_str()); }
	));

	g->add (V::digit, rge('0', '9'));
	
	g->add (V::string, seq(
				'\"', markb, many(0,0 , ngrp("\"")), marke, '\"', V::SPACES //,    DO { str = yy; }
	));

	g->add (V::relop, alt(
				  seq ("<=", V::SPACES, DO { op = lessEqual; })
				, seq ("<>", V::SPACES, DO { op = notEqual; })
				, seq ('<', V::SPACES,  DO { op = lessThan; })
				, seq (">=", V::SPACES, DO { op = greaterEqual; })
				, seq ('>', V::SPACES,  DO { op = greaterThan; })
				, seq ('=', V::SPACES,  DO { op = equalTo; })
	));
	
	g->add (V::EQUAL, seq ('=', V::SPACES));
	g->add (V::CLOSE, seq (')', V::SPACES));
	g->add (V::OPEN,  seq ('(', V::SPACES));
	g->add (V::SLASH, seq ('/', V::SPACES));
	g->add (V::STAR,  seq ('*', V::SPACES));
	g->add (V::MINUS, seq ('-', V::SPACES));
	g->add (V::PLUS,  seq ('+', V::SPACES));
	g->add (V::COMMA, seq (',', V::SPACES));
	g->add (V::CR,    alt ('\n', '\r', "\r\n"));
	g->add (V::SPACES, many(0,0, grp (" \t")));

	return g;
}

/*/// ==================================================================================================================================
/*/// ----------------------------------------------------------------------------------------------------------------------------------
void accept (size_t l, string& t) {
	if (t.empty() || t[0] < 32) {
		auto f = lines.find (l);
		if (f != lines.end()) lines.erase(f);
	}
	else {
		size_t i = 0;
		while (isspace(t[i])) { ++i; if (i == t.size()) return; }
		lines [l] = t.substr(i, t.size()-i);
	}
}

inline string extend (const string& name) {
	string tail = name.substr (name.size()-4, 4);
	string extend = name;
	if (tail != ".bas") { extend .append (".bas"); }
	return extend;
}

void save (const string& name) {
	string file = extend (name);
	ofstream out (file.c_str());
	if (out.fail()) {
		cerr << "Unable to open " << file.c_str() << " for writing.\n";
	}
	else {
		for (auto i = lines.begin(); i != lines.end(); ++i) {
			cout << (*i).first << " " << (*i).second.c_str();
		}
		out.close();
	}
}

void load (const string& name) {
	string file = extend (name);
	ifstream in (file.c_str());
	if (in.fail()) {
		cerr << "Unable to open " << file.c_str() << endl;
	}
	else {
		while (!in.eof()) {
			size_t lnum = 0;
			string s;
			
			in >> lnum;
			getline (in, s);
			s.append ("\n");
			
			accept (lnum, s);
		}
		in.close();
	}
}

void type (const string& name) {
	string file = extend (name);
	
	ifstream in (file.c_str());
	if (in.fail()) {
		cerr << "Unable to open " << file.c_str() << endl;
	}
	else {
		char d;
		while (!in.eof()) {
			d = in.get();
			cout << d;
		}
		in.close();
		if (d != '\n' && d != '\r') { cout << endl; }
	}
	
}

int input (void) {
	string ln;
	getline (cin, ln);
	return atoi (ln.c_str());
}

bool set_code_from_line (size_t l) {
	if (!l) {
		if (lines.empty()) return false;
		auto i = lines.begin();
		pc = (*i).first;
		code = &(*i).second;
	}
	else {
		auto i = lines.find (l);
		if (i == lines.end()) return false;
		pc = l;
		code = &(*i).second;
	}
	
	codestream .set_line (*code);
	return true;
}



/*/// ==================================================================================================================================
/*/// ----------------------------------------------------------------------------------------------------------------------------------
const char* totext[] = {
	"nil", "line", "statement", "exprlist", "varlist", "expression", "term", "factor", "var", "number", "digit", "string",
	"relop", "EQUAL", "CLOSE", "OPEN", "SLASH", "STAR", "MINUS", "PLUS", "COMMA", "SPACES", "CR"
};

struct bslog : public logger<char> {
	virtual void log_rule (size_t ruleno) {
		cout << ">>> rule: " << totext[ruleno] << endl;
	}
	
	virtual void log_single (const char& ch, const char* msg) {
		cout << msg << ": " << ch << endl;
	}
	
	virtual void log_sequence (const char* seq, const char* msg) {
		cout << msg << ": " << seq << endl;
	}
	
	virtual void log_message (const char* msg) {
		cout << msg << endl;
	}
	
	virtual void log_result (bool r) {
		cout << std::boolalpha;
		cout << r << endl;
	};
};

size_t find_nextline (size_t apc) {
	auto i = lines.find (apc);
	++i;
	if (i == lines.end()) return 0;
	return (*i).first;
}

bool gonextline () {
	if (!inprog) return false;
	auto i = lines.find (pc);
	++i;
	if (i == lines.end()) { return false; }
	pc = (*i).first;
	code = &(*i).second;
	codestream .set_line (*code);
	return true;
}

void run (basicGrammar& bg) {
	if (!bg.parse (V::line, codestream)) return;
	bg.apply_actions();
	
	while(inprog && bg.parse (V::line, codestream)) {
		bg.apply_actions();
		if (!gopc) { gonextline(); }
		gopc = 0;
//		bg.reset();
	}

	bg.apply_actions();
}

/*/// ==================================================================================================================================
/*/// ----------------------------------------------------------------------------------------------------------------------------------
int main(int argc, const char * argv[]) {
	if (argc > 1) {
		batch = true;
		while (argc-- > 1) { string f (*++argv); load (f); }
		pc = 0;
	}
	
	basicGrammar* bg = create_grammar();
	bslog   logger;
//	bg ->set_logger (logger);
	
	while (!cin.eof()) {
		string s;
		cout << "> ";
		getline (cin, s);
		s.append("\n");
		codestream .set_line (s);
		::run (*bg);
	}
	return 0;
}

