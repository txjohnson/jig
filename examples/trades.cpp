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
#define JIG_LOGGING
#include "jig.h"
using namespace std;
using namespace jig;


/*/// ==================================================================================================================================
	A grammar to parse a slightly extended version of the trading language described in DSL's in Action (Debasish Ghosh, 2011)
/*/// ----------------------------------------------------------------------------------------------------------------------------------
namespace V {
	const int statement   = 1;
	const int details     = 2;
	const int detail      = 3;
	const int action      = 4;
	const int client      = 5;
	const int security    = 6;
	const int limit       = 7;
	const int quantity    = 8;
	const int lot         = 9;
	const int digit       = 10;
	const int identifier  = 11;
	const int execute     = 12;
	const int request     = 13;
	const int number      = 14;
	const int alpha       = 15;
	const int EQUAL       = 16;
	const int PERIOD      = 17;
	const int SPACES      = 18;
	const int COMMA       = 19;
	const int CR          = 20;
};

jig::markbegin markb;
jig::markend   marke;
jig::any       wildcard;

/*/// ==================================================================================================================================
/*/// ----------------------------------------------------------------------------------------------------------------------------------
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

/*/// ==================================================================================================================================
/*/// ----------------------------------------------------------------------------------------------------------------------------------
struct stringInput {
	typedef char value_t;
	
	stringInput () { }
	~stringInput () { }
	
	value_t value() { while (pos >= str.size()) fetch(); return str[pos]; }
	bool advance () { while (pos >= str.size()) fetch(); ++pos; return true; }
	size_t  tell () { return pos; }
	void seek (size_t where) { pos = where; }
	
	bool eof ()     { return false; }
	
	void fetch () {
//		string s;
//		getline (cin, s);
//		str .append (s);
		str .push_back (cin.get());
	}
	
	string str;
	size_t pos = 0;
};

stringInput input;

/*/// ==================================================================================================================================
/*/// ----------------------------------------------------------------------------------------------------------------------------------
enum class action_t { unspecified, buy, sell };
enum class limit_t  { unspecified, exactly, limited };
enum class count_t  { unspecified, exactly, upto };

class Order {
public:

	Order () : action (action_t::unspecified), count (count_t::unspecified), limit (limit_t::unspecified),
				no_shares (0), lot_size (0), price (0) { }

	bool check () {
		bool no_errors = true;
		if (client.empty())                { cout << "Error. No client specified.\n"; no_errors = false; }
		if (security.empty())              { cout << "Error. No security specified.\n"; no_errors = false; }
		if (count == count_t::unspecified) { cout << "Error. No number of shares specified.\n"; no_errors = false; }
		if (limit == limit_t::unspecified) { cout << "Warning. No share price or limit specified.\n"; }
		return no_errors;
	}
	
	void review () {
		cout << "\t";
		cout << ((action == action_t::buy) ? "buy " : "sell ");
		if (count == count_t::upto)  { cout << "upto "; }
		cout << no_shares << " shares of " << security.c_str() << " at ";
		if (limit == limit_t::limited) cout << "limit price = ";
		if (limit == limit_t::unspecified) cout << "market price"; else cout << price;
		cout << " for client: " << client.c_str() << endl;
	}
	
	action_t action;
	string   security;
	string   client;
	count_t  count;
	size_t   no_shares;
	size_t   lot_size;
	limit_t  limit;
	size_t   price;
};

Order  work;
bool   cancel;
bool   ready;
size_t num;

/*/// ==================================================================================================================================
	
	action:          buy | sell
	client phrase:   for name
	lot phrase:      in lots of number
	amt phrase:      number shares | up to number
	limit phrase:    at number | where limit price = number
	security-phrase: of security
	
	phrase:          action-stmt details execute
	
	action-stmt:     action (client phrase | lot phrase | amt phrase | limit phrase | secuity phrase)+ PERIOD
	
	detail:          action? (client phrase | lot phrase | amt phrase | limit phrase | secuity phrase)+ PERIOD
	
	
	
/*/// ----------------------------------------------------------------------------------------------------------------------------------
typedef jig::grammar<stringInput> basicGrammar;
typedef basicGrammar::regs_t  regs_t;
#define DO [](basicGrammar::regs_t& yy, bool& yythunk)->void


basicGrammar* create_grammar () {
	basicGrammar* g = new basicGrammar ();
	
	g->add (V::statement, seq(V::SPACES, alt(seq(V::action,
			many(1,0, alt(V::client, V::security, V::limit, V::quantity, V::lot)),
			V::PERIOD, DO { ready = true; }),
			seq(alt("quit", "bye", "exit"),                                  DO { exit(0); }),
			seq(markb, many(0,0, seq (ntst(V::CR), wildcard)), V::CR, marke, DO { cout << "Syntax error\n"; cancel = true; })
	)));
	
	g->add (V::detail, seq( V::SPACES, alt(
		seq(opt(V::action), many (1,0, alt(V::client, V::security, V::limit, V::quantity, V::lot)), V::PERIOD),
		seq (V::execute,                                                 DO { if (work.check()) ready = false; else cancel = true; }),
		seq ("cancel",                                                   DO { cancel = true;}),
		seq(alt("quit", "bye", "exit"),                                  DO { exit(0); }),
		seq(markb, many(0,0, seq (ntst(V::CR), wildcard)), V::CR, marke, DO { cout << "Syntax error\n"; cancel = true; })
		)));
	
	g->add (V::client,    seq("for", V::SPACES, V::identifier,
				DO {
					if (!work.client.empty()) { cout << "changing client to: " << yy.text.c_str() << endl; }
					work.client = yy.text;
				}));
	
	g->add (V::security,  seq("of" , V::SPACES, V::identifier,
				DO {
					if (!work.security.empty()) { cout << "changing security to: " << yy.text.c_str() << endl; }
					work.security = yy.text;
				}));
	
	g->add (V::limit,     alt(seq("at", V::SPACES, V::number,
				DO {
					if (work.limit == limit_t::unspecified) {
						cout << "changing limit to: at " << num << endl; }
					work.limit = limit_t::exactly;
					work.price = num;
				}),
			seq("where", V::SPACES, "limit", V::SPACES, "price", V::SPACES, V::EQUAL, V::number,
				DO {
					if (work.limit == limit_t::unspecified) {
						cout << "changing limit to: where limit price = " << num << endl; }
					work.limit = limit_t::limited;
					work.price = num;
				})));
	
	g->add (V::quantity,  alt(
		seq (V::number, "shares", V::SPACES,
				DO {
					if (work.count != count_t::unspecified) {
						cout << "changing count to: exactly " << num << " shares\n"; }
					work.count = count_t::exactly;
					work.no_shares = num;
				}),
		seq ("up", V::SPACES, "to", V::SPACES, V::number, "shares", V::SPACES,
				DO {
					if (work.count != count_t::unspecified) {
						cout << "changing count to: upto " << num << " shares\n"; }
					work.count = count_t::upto;
					work.no_shares = num;
				})
	));
	
	g->add (V::lot, seq("in", V::SPACES, "lots", V::SPACES, "of", V::SPACES, V::number,
				DO {	if (work.lot_size) { cout << "changing lot size to: " << num << endl; }
						work.lot_size = num; }));
	
	g->add (V::action,
		seq(alt(seq("buy",     DO { if (work.action == action_t::unspecified || work.action == action_t::buy) {
										work.action = action_t::buy;
									}
									else { cout << "buy and sell cannot be mixed in the same order.\n"; yythunk = true; cancel = true; }
								}),
		        seq("sell",    DO { if (work.action == action_t::unspecified || work.action == action_t::sell) {
										work.action = action_t::sell;
									}
									else { cout << "buy and sell cannot be mixed in the same order.\n"; yythunk = true; cancel = true; }
								})
			), V::SPACES));
	
	g->add (V::number, seq (markb, many(1,0, V::digit), marke, V::SPACES,       DO { num = atoi(yy.text.c_str()); }
	));
	
	g->add (V::execute, seq ("execute", DO { ready = true; }));

	g->add (V::digit, rge('0', '9'));
	
	g->add (V::alpha, alt(rge('a', 'z'), rge('A', 'Z')));
	
	g->add (V::identifier, seq(markb, many(1,0 , V::alpha), marke, V::SPACES));
	
	g->add (V::EQUAL, seq ('=', V::SPACES));
	g->add (V::PERIOD, seq ('.', V::SPACES));
	g->add (V::COMMA, seq (',', V::SPACES));
	g->add (V::SPACES, many(0,0, grp (" \t\r\n")));
	g->add (V::CR,    alt ('\n', '\r', "\r\n"));

	return g;
}

/*/// ==================================================================================================================================
/*/// ----------------------------------------------------------------------------------------------------------------------------------

/*/// ==================================================================================================================================
/*/// ----------------------------------------------------------------------------------------------------------------------------------
const char* totext[] = {
	"nil", "statement", "details", "detail", "action", "client", "security", "limit", "quantity", "lot", "digit",
	"identifier", "execute", "request", "number", "alpha", "EQUAL", "PERIOD", "SPACES", "COMMA", "CR"
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

//
// Simple stateful handling of parser
bool get_order (basicGrammar& bg) {
	work = Order ();
	cancel = false; ready = false;
	
	// start of statement with mandatory action
	if (bg.parse (V::statement, input)) {
		bg.apply_actions();
	}
	
	if (cancel) return false;
	
	// multiple elaboration statements
	while (ready) {
		if (!bg.parse (V::detail, input) || cancel) {
			bg.apply_actions();
			return false;
		}
		bg.apply_actions();
	}
	
	//execute statement
	cout << "This is your order:\n\n";
	work.review();
	cout << "\nEnter yes to submit...";
	string confirm;
	getline (cin, confirm);
	
	if (confirm == "yes") {
		// submit order....
		cout << "submitted.\n\n";
	}
	
	return true;
}

/*/// ==================================================================================================================================
/*/// ----------------------------------------------------------------------------------------------------------------------------------
int main(int argc, const char * argv[]) {
	basicGrammar* bg = create_grammar();

	bslog   logger;
//	bg ->set_logger (logger);
	
	while (!cin.eof()) {
		cout << "Enter an order.\n\n";
		get_order (*bg);
	}
	return 0;
}

