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

#ifndef jig_header
#define jig_header

#include <vector>
#include <string>
#include <functional>

/*/// ==================================================================================================================================
	Parsing Expressions. Rewrite of a previous project using C++11 features.
	
	The parser works on a stream of objects. A stream is very simple and supports these basic operations:
		value_t value ()            // get the value at the current position. does not advance the stream
		bool    advance ()          // advance the stream. report fail if stream cannot advance, true otherwise
		size_t  tell   ()           // report position
		void    seek   (size_t p);  // absolute seeking only
		value_t                     // reports the character type of the stream
		vool    eof() 

	So below, S will be used to represent the stream object
	
	Other abbreviations:
		G - goal. a pattern that we want to match
		GR - grammar. The grammar object containing productions
		A, B, C, U, T - template variables with no meaning other than placeholders in patterns (unless otherwise noted)
/*/// ----------------------------------------------------------------------------------------------------------------------------------

/*/// ----------------------------------------------------------------------------------------------------------------------------------
	JIG_PUBLISHED - for client use (methods and members of a class)
	JIG_PUBLIC    - for client use (templates, enums and structs)
	Anything else (methods, members, templates, etc) are implementation details and are subject to change
/*/// ----------------------------------------------------------------------------------------------------------------------------------
#define JIG_PUBLISHED public
#define JIG_PUBLIC

namespace jig {

/*/// ==================================================================================================================================
	A primitive is an object that either matches a part of a stream directly or performs some user action.

	The basic primitives: 
		match  - match a single item
		word   - match a specific sequence of items  value_t(0) is taken to be constructor for null terminator
		set    - match one item in a group
		range  - match an item that falls within a inclusive range. (items must be support >= and <=)
		action - perform some user action
		
	Not so basic but necessary:
		call   - match a production
		
	Primitives need to define
		bool operator()(S&) - for performing matching
	
/*/// ----------------------------------------------------------------------------------------------------------------------------------

/*/// ----------------------------------------------------------------------------------------------------------------------------------
/*/// ----------------------------------------------------------------------------------------------------------------------------------
#ifdef JIG_LOGGING

	template <typename CHAR> struct logger {
		virtual void log_rule (size_t ruleno) = 0;
		virtual void log_single (const CHAR& ch, const char* msg) = 0;
		virtual void log_sequence (const CHAR* seq, const char* msg) = 0;
		virtual void log_message (const char* msg) = 0;
		virtual void log_result (bool) = 0;
	};
	
#define LOG_SINGLE(X,Y)   if (g.logger()) g.logger()->log_single (X, Y);
#define LOG_SEQUENCE(X,Y) if (g.logger()) g.logger()->log_sequence (X, Y);
#define LOG_SUCCESS       if (g.logger()) g.logger()->log_result (true);
#define LOG_FAIL          if (g.logger()) g.logger()->log_result (false);
#define LOG_MSG(X)        if (g.logger()) g.logger()->log_message (X);
#define LOG_RULE(X)       if (g.logger()) g.logger()->log_rule (X);
#else
#define LOG_SINGLE(X,Y)
#define LOG_SEQUENCE(X,Y)
#define LOG_SUCCESS
#define LOG_FAIL
#define LOG_MSG(X)
#define LOG_RULE(X)
#endif

/*/// ----------------------------------------------------------------------------------------------------------------------------------
/*/// ----------------------------------------------------------------------------------------------------------------------------------
	JIG_PUBLIC template <typename G> struct match {

		match (const G& t) : goal (t) { }
		~match () { }
		
		template <typename S, typename GR> bool operator() (S& s, GR& g, size_t rule, bool left) {
	#ifndef MFPE_NO_CONSTRAINT_CHECKS
			// enforce value_t of S is compatible with G. hopefully gives meaningful compiler error when doesn't
			typename S::value_t constraint = G(0); (void)constraint;
	#endif

			LOG_SINGLE(goal, "try match")
			if (left) { LOG_FAIL return false; }
			if (s.value() == goal) { s.advance(); LOG_SUCCESS return true; }
			LOG_FAIL
			return false;
		}
		
		bool is_null_goal () { return false; }
		G goal;
	};

	JIG_PUBLIC template <typename G> struct word {

		word (const G* aword) : goal (aword) { }
		~word () { }

		template <typename S, typename GR> bool operator() (S& s, GR& g, size_t rule, bool left) {
	#ifndef MFPE_NO_CONSTRAINT_CHECKS
			// enforce value_t of S is compatible with G. hopefully gives meaningful compiler error when doesn't
			typename S::value_t constraint = G(0); (void)constraint;
	#endif
			LOG_SEQUENCE(goal, "try word")
			if (left) { LOG_FAIL return false; }
			G nil (0);
			auto i = goal;
			while (*i == s.value()) { ++i; if (!s.advance()) break; }
			if (*i == nil) { LOG_SUCCESS return true; }
			LOG_FAIL
			return false;
		}

		bool is_null_goal () { return false; }
		const G* goal;  // null terminated
	};

	JIG_PUBLIC template <typename G> struct group {
		group (bool negated, const G* aword) : goal (aword), negate (negated) { }
		~group () { }

		template <typename S, typename GR> bool operator() (S& s, GR& g, size_t rule, bool left) {
	#ifndef MFPE_NO_CONSTRAINT_CHECKS
			// enforce value_t of S is compatible with G. hopefully gives meaningful compiler error when doesn't
			typename S::value_t constraint = G(0); (void)constraint;
	#endif
			LOG_SEQUENCE(goal, "try group")
			if (left) { LOG_FAIL return false; }
			G nil (0);
			auto i = goal;
			
			if (negate) {
				LOG_MSG("group is negated");
				while (!(*i == s.value())) {
					++i;
					if (*i == nil) {
						s.advance();
						LOG_SUCCESS
						return true;
					}
				}
				LOG_FAIL
				return false;
			}
			else {
				while (!(*i == s.value())) {
					++i;
					if (*i == nil) { LOG_FAIL return false; }
				}
				s.advance();
				LOG_SUCCESS
				return true;
			}
		}

		bool is_null_goal () { return false; }
		const G* goal;  // null terminated
		bool negate;
	};

	JIG_PUBLIC template <typename G> struct range {
		range (const G& low_value, const G& high_value) : low (low_value), high (high_value) {
		}
		~range () { }

		template <typename S, typename GR> bool operator() (S& s, GR& g, size_t rule, bool left) {
	#ifndef MFPE_NO_CONSTRAINT_CHECKS
			// enforce value_t of S is compatible with G. hopefully gives meaningful compiler error when doesn't
			typename S::value_t constraint = G(0); (void)constraint;
	#endif
			LOG_MSG("range check")
			LOG_SINGLE( low, "low value")
			LOG_SINGLE( high, "high value")
			LOG_SINGLE( s.value(), "comparing")
			if (left) { LOG_FAIL return false; }
			if (low <= s.value() && s.value() <= high) { s.advance(); LOG_SUCCESS return true; }
			LOG_FAIL
			return false;
		}

		bool is_null_goal () { return false; }
		G low;
		G high;
	};

	JIG_PUBLIC template <typename F, typename T, typename S, typename GR> struct userrule {
		userrule (const T& afunction) : op (afunction) { }
		~userrule () { }
		
		bool operator() (S& s, GR& g, size_t rule, bool left) {
			LOG_MSG("--userrule")
			if (op (s, g, rule, left)) { LOG_SUCCESS return true; }
			LOG_FAIL
			return false;
		}
		
		bool is_null_goal () { return false; }
		std::function<F> op;
	};


	JIG_PUBLIC struct call {

		call (size_t prod_rule_num) : goal (prod_rule_num) { }
		~call () { }
		
		template <typename S, typename GR> bool operator() (S& s, GR& g, size_t rule, bool left) {
			LOG_MSG("--call")
			if (left) { if (rule == goal) { LOG_SUCCESS return true; } else { LOG_FAIL return false; } }
			if (g .parse (goal, s)) { LOG_SUCCESS return true; }
			LOG_FAIL
			return false;
		}
		
		bool is_null_goal () { return false; }
		size_t   goal;
	};

	JIG_PUBLIC struct markbegin {
		markbegin () { }
		~markbegin () { }

		template <typename S, typename GR> bool operator() (S& s, GR& g, size_t rule, bool left) {
			LOG_MSG("--mark begin") g.begin_saving_items (s); LOG_SUCCESS return true;
		}

		bool is_null_goal () { return true; }
	};

	JIG_PUBLIC struct markend {
		markend () { }
		~markend () { }
		
		template <typename S, typename GR> bool operator() (S& s, GR& g, size_t rule, bool left) {
			LOG_MSG("--mark end") g.end_saving_items (s); LOG_SUCCESS return true;
		}
		
		bool is_null_goal () { return true; }
	};
	
	JIG_PUBLIC struct any {
		any () { }
		~any () { }
		
		template <typename S, typename GR> bool operator() (S& s, GR& g, size_t rule, bool left) {
			LOG_MSG("--any")
			if (left) { LOG_FAIL return false; }
			if (s.eof()) { LOG_FAIL return false; }
			s.advance();
			LOG_SUCCESS
			return true;
		}

		bool is_null_goal () { return false; }
	};

	template <typename F, typename T> struct action {

		action (const T& afunction) : op (afunction) { }
		~action () { }
		
		template <typename S, typename GR> bool operator() (S& s, GR& g, size_t rule, bool left) {
			LOG_MSG("--action")
			if (left) { LOG_FAIL return false; }
			g.queue_action (op);
			LOG_SUCCESS
			return true;
		}

		bool is_null_goal () { return true; }
		std::function<F> op;
	};

	template <typename F, typename T> struct immediate {

		immediate (const T& afunction) : op (afunction) { }
		~immediate () { }
		
		template <typename S, typename GR> bool operator() (S& s, GR& g, size_t rule, bool left) {
			LOG_MSG("--immediate")
			bool thunk = false;
			if (left) { LOG_FAIL return false; }
			op (g.get_state(), thunk);
			if (thunk) { LOG_FAIL return false; }
			LOG_SUCCESS
			return true;
		}

		bool is_null_goal () { return true; }
		std::function<F> op;
	};

/*/// ==================================================================================================================================
	Lift. Here we lift the forms that have special meaning to our framework. Anything else will crap out
/*/// ----------------------------------------------------------------------------------------------------------------------------------
	template <typename T> struct lift {

		template <typename U> struct other : public other <decltype(&U::operator())> {
		};

		template <typename C, typename A> struct other <void(C::*)(A,bool&) const> {
			typedef action<void(A,bool&), C> inner;
		};
		
		template <typename A> struct other <void(A,bool&)> {
			typedef action<void(A,bool&), void(A,bool&)> inner;
		};

		template <typename C, typename A> struct other <void(C::*)(A,bool&,bool) const> {
			typedef immediate<void(A,bool&,bool), C> inner;
		};
		
		template <typename A> struct other <void(A,bool&,bool)> {
			typedef immediate<void(A,bool&,bool), void(A,bool&,bool)> inner;
		};
		
		template <typename C, typename A, typename B> struct other <bool(C::*)(A&, B&, size_t, bool)> {
			typedef userrule <bool(A&,B&,size_t,bool), bool(A&,B&,size_t,bool), A, B> inner;
		};
		
		template <typename A, typename B> struct other <bool(A&, B&, size_t, bool)> {
			typedef userrule <bool(A&,B&,size_t,bool), bool(A&,B&,size_t,bool), A, B> inner;
		};
		
			
		typedef typename other<T>::inner type;
	};


	template <> struct lift <char>                         { typedef match <char> type; };

	template <> struct lift <wchar_t>                      { typedef match <wchar_t> type; };

	template <> struct lift <const char*>                  { typedef word <char> type; };
	template <size_t N> struct lift <char const (&)[N]>    { typedef word <char> type; };

	template <> struct lift <const wchar_t*>               { typedef word <wchar_t> type; };
	template <size_t N> struct lift <wchar_t const (&)[N]> { typedef word <wchar_t> type; };

	template <> struct lift <int>                          { typedef call type; };
	template <> struct lift <const int&>                   { typedef call type; };

	template <typename A> struct lift <void(*)(A&, bool&)>        { typedef action<void(A&, bool&), void(*)(A&,bool&)> type; };

	template <typename A> struct lift <void(&)(A&, bool&)>        { typedef action<void(A&, bool&), void(&)(A&,bool&)> type; };

	template <typename A> struct lift <void(*)(A&, bool&, bool)>  { typedef action<void(A&, bool&, bool), void(*)(A&,bool&,bool)> type; };

	template <typename A> struct lift <void(&)(A&, bool&, bool)>  { typedef action<void(A&, bool&, bool), void(&)(A&,bool&,bool)> type; };
	
	template <typename A, typename B> struct lift <bool(*)(A&,B&,size_t,bool)> {
		typedef userrule<bool(A&,B&,size_t,bool), bool(*)(A&,B&,size_t,bool), A, B> type;
	};
	
	template <typename A, typename B> struct lift <bool(&)(A&,B&,size_t,bool)> {
		typedef userrule<bool(A&,B&,size_t,bool), bool(&)(A&,B&,size_t,bool), A, B> type;
	};

	template <typename...E> class sequence;
	template <typename ...E> class alternance;
	template <typename A> struct repeat;
	template <typename A> struct test;
	template <typename A> struct option;

	template <typename...A> struct lift <sequence<A...>>   { typedef typename sequence<A...>::type type; };

	template <typename...A> struct lift <alternance<A...>> { typedef typename alternance<A...>::type type; };

	template <typename A> struct lift <repeat<A>>          { typedef typename repeat<A>::type type; };

	template <typename A> struct lift <test<A>>            { typedef typename test<A>::type type; };

	template <typename A> struct lift <match<A>>           { typedef match<A> type; };

	template <typename A> struct lift <word<A>>            { typedef word<A> type; };

	template <typename A> struct lift <group<A>>           { typedef group<A> type; };

	template <typename A> struct lift <range<A>>           { typedef range<A> type; };

	template <typename A> struct lift <option<A>>          { typedef option<A> type; };
	
	template <typename A, typename B, typename C, typename D> struct lift <userrule<A,B,C,D>> { typedef userrule<A,B,C,D> type; };
	
	template <typename A, typename B> struct lift <immediate<A,B>> { typedef immediate<A, B> type; };

	template <> struct lift <call>                         { typedef call type; };
	template <> struct lift <call&>                        { typedef call type; };

	template <> struct lift <markbegin>                    { typedef markbegin type; };
	template <> struct lift <markbegin&>                   { typedef markbegin type; };

	template <> struct lift <markend>                      { typedef markend type; };
	template <> struct lift <markend&>                     { typedef markend type; };

	template <> struct lift <any>                          { typedef any type; };
	template <> struct lift <any&>                         { typedef any type; };


/*/// ==================================================================================================================================
	Sequences and Alternation are primitives that hold a collection of other primitives.These two classes infer the range
	they operate on from their arguments.
	/*/// ----------------------------------------------------------------------------------------------------------------------------------
	JIG_PUBLIC template <typename H, typename ... T> class sequence <H, T...> : private sequence <T...> {
		typedef sequence<T...> inherited;
		
	public:
		typedef sequence <H, T...> type;

		sequence () { }
		sequence (const H& h, const T&... t) : goal(typename lift<H>::type (h)), inherited (t...) { }
		template<typename...V> sequence (const sequence<V...>& a) : goal (a.goal), inherited (a.tail()) { }
		~sequence () { }
		
		template <typename S, typename GR> bool operator() (S& s, GR& g, size_t rule, bool left) {
			LOG_MSG("**sequence")
			if (goal (s, g, rule, left)) {
				inherited& nextgoal = tail();
				if (goal.is_null_goal()) {
					LOG_MSG("****next")
					if (nextgoal (s, g, rule, left)) { LOG_SUCCESS return true; }
				}
				else {
					LOG_MSG("****next")
					if (nextgoal (s, g, 0, false)) { LOG_SUCCESS return true; }
				}
			}
			LOG_FAIL
			return false;
		}

		bool is_null_goal () { return false; }
			
	private:
		inherited& tail() { return *this; }
		const inherited& tail() const { return *this; }

		typename lift<H>::type goal;
	};

	template <> class sequence <> {
	public:
		template <typename S, typename GR> bool operator() (S& s, GR& g, size_t rule, bool left) {
			LOG_MSG("**sequence end")
			LOG_SUCCESS
			return true;
		}
	};

/*/// ----------------------------------------------------------------------------------------------------------------------------------
/*/// ----------------------------------------------------------------------------------------------------------------------------------
	JIG_PUBLIC template <typename H, typename ...T> class alternance <H, T...> : private alternance <T...> {
		typedef alternance<T...> inherited;
		
	public:
		typedef alternance<H, T...> type;
		
		alternance (const H& h, const T&... t) : goal(typename lift<H>::type (h)), inherited (t...) { }
		template<typename...V> alternance (const alternance<V...>& a) : goal (a.goal), inherited (a.tail()) { }
		~alternance () { }
		
		template <typename S, typename GR> bool operator() (S& s, GR& g, size_t rule, bool left) {
			LOG_MSG ("**alternance")
			size_t pos = s.tell();
			size_t a = g.action_queue_state ();
			
			if (goal(s, g, rule, left)) { LOG_SUCCESS return true; }
			s.seek (pos);
			g.set_action_queue_state (a);
			
			LOG_MSG("**** trying alternate")
			if (tail()(s, g, rule, left)) { LOG_SUCCESS return true; }
			LOG_FAIL
			return false;
		}
		
		bool is_null_goal () { return false; }

	protected:
		inherited& tail() { return *this; }
		const inherited& tail() const { return *this; }

		typename lift<H>::type goal;
	};

	template <> class alternance <> {
	public:
		template <typename S, typename GR> bool operator() (S& s, GR& g, size_t rule, bool left) {
			LOG_MSG("**alternance end")
			LOG_FAIL
			return false;
		}
	};

/*/// ----------------------------------------------------------------------------------------------------------------------------------
/*/// ----------------------------------------------------------------------------------------------------------------------------------
	JIG_PUBLIC template <typename G> struct repeat {
		typedef repeat<G> type;
		
		repeat (size_t low_bound, size_t high_bound, const G& agoal) : goal (typename lift<G>::type(agoal)), low (low_bound), high (high_bound) { }
		~repeat () { }

		template <typename S, typename GR> bool operator() (S& s, GR& g, size_t rule, bool left) {
			size_t count = 0;
			
			LOG_MSG("--repeat")
			if (left) { LOG_FAIL return false; }
			if (!goal (s, g, rule, left)) { if (!low) { LOG_SUCCESS return true; } else { LOG_FAIL return false; } }
			++count;
			
			size_t n = s.tell();
			while (goal (s, g, rule, false)) { ++ count; n = s.tell(); }
			s.seek(n);
			if (count >= low) { LOG_SUCCESS return true; }
			if (!high)        { LOG_SUCCESS return true; }
			if (count < high) { LOG_FAIL return false; }
			LOG_SUCCESS
			return true;
		}
		
		bool is_null_goal () { return false; }
		size_t low;
		size_t high;
		typename lift<G>::type goal;
	};

	JIG_PUBLIC template <typename G> struct test {
		typedef test<G> type;
		
		test (bool negated, const G& agoal) : goal (typename lift<G>::type(agoal)), negate (negated) { }
		~test () { }
		
		template <typename S, typename GR> bool operator() (S& s, GR& g, size_t rule, bool left) {
			size_t n = s.tell();
			LOG_MSG("--test")
			if (left) { LOG_FAIL return false; }

			bool r = goal (s, g, 0, false);
			s.seek(n);
			if (r) { if (negate) { LOG_MSG("(negated)") LOG_FAIL return false;  } LOG_SUCCESS return true; }
			if (negate) { LOG_MSG("(negated") LOG_SUCCESS return true; }
			LOG_FAIL
			return false;
		}
		
		bool is_null_goal () { return true; }
		typename lift<G>::type goal;
		bool    negate;
	};

	JIG_PUBLIC template <typename G> struct option {
		typedef option<G> type;
		
		option (const G& agoal) : goal (typename lift<G>::type(agoal)) { }
		~option () { }
		 
		template <typename S, typename GR> bool operator() (S& s, GR& g, size_t rule, bool left) {
			LOG_MSG("--option")
			if (left) { LOG_FAIL return false; }

			size_t n = s.tell();
			bool r = goal (s, g, rule, left);
			if (!r) s.seek(n);
			LOG_SUCCESS
			return true;
		}
		
		bool is_null_goal () { return false; }
		typename lift<G>::type goal;
	};

/*/// ----------------------------------------------------------------------------------------------------------------------------------
	objects that can construct sequences, alternances, calls, actions...
/*/// ----------------------------------------------------------------------------------------------------------------------------------
	JIG_PUBLIC template <typename... Types> sequence <Types...> seq (Types&&... t) {
		return sequence <Types...> (t...);
	}

	JIG_PUBLIC template <typename... Types> alternance <Types...> alt (Types&&... t) {
		return alternance <Types...> (t...);
	}
	
	JIG_PUBLIC template <typename G> test<G>   tst (const G& goal) { return test<G> (false, goal); }
	JIG_PUBLIC template <typename G> test<G>   ntst (const G& goal) { return test<G> (true, goal); }

	JIG_PUBLIC template <typename G> repeat<G> many (size_t low, size_t hi, const G& goal) {
		return repeat<G> (low, hi, goal);
	}

	JIG_PUBLIC template <typename G> option<G> opt (const G& goal) { return option<G> (goal); }
	
	JIG_PUBLIC template <typename G> range<G> rge (G low, G hi) { return range<G> (low, hi); }
	
	JIG_PUBLIC template <typename G> group<G> grp (const G* set) { return group<G>(false, set); }
	JIG_PUBLIC template <typename G> group<G> ngrp (const G* set) { return group<G>(true, set); }

/*/// ==================================================================================================================================
	A production associates a number with a primitive. Numbers are specified by the client. It's recommended that the numbers go in
	sequence beginning at 1. 0 is reserved.
/*/// ----------------------------------------------------------------------------------------------------------------------------------
	template <typename GRAMMAR> struct abproduction {
	public:
		abproduction () : pnum (0) { }
		abproduction (size_t anumber) : pnum (anumber) { }
		virtual ~abproduction () { }
		
		virtual bool operator()(typename GRAMMAR::stream_t& s, GRAMMAR& g, bool left) = 0;

		size_t identifier () { return pnum; }
	protected:
		size_t  pnum;
	};

	JIG_PUBLIC template <typename GRAMMAR, typename G> class production : public abproduction <GRAMMAR> {
	public:
		typedef abproduction<GRAMMAR> inherited;
		production (size_t number, const G& agoal) : abproduction<GRAMMAR>(number), goal (agoal) { }
		virtual ~production () { }
		
		virtual bool operator()(typename GRAMMAR::stream_t& s, GRAMMAR& g, bool left) {
			size_t& p = g.get_memo (this->pnum);
			size_t n = s.tell();
			size_t m = 0;
			size_t a = g.action_queue_state ();
			
			if (!left) {
				if (p == (s.tell()+1))
					return false;
				m = p;
				p = s.tell() + 1;
				if (goal (s, g, this->pnum, false)) goto on_success;
				p = m;
				s.seek(n);
				g.set_action_queue_state (a);
				return false;
			}
			else {
				if (goal (s, g, this->pnum, true)) goto on_success;
				s.seek(n);
				g.set_action_queue_state (a);
				return false;
			}
			
			on_success:
			p = m;
			(*this)(s, g, true);
			return true;
		}
		
	protected:
		G goal;
	};

/*/// ----------------------------------------------------------------------------------------------------------------------------------
	Determined our string type for sending matches to actions
/*/// ----------------------------------------------------------------------------------------------------------------------------------
	template <typename T> struct matched {
		typedef std::vector <T> type;
	};

	template <> struct matched <char> {
		typedef std::string type;
	};

	template <> struct matched <wchar_t> {
		typedef std::wstring type;
	};

	template <typename A, typename B> struct regpack {
		A text;
		B user;
	};
	
	struct nilregisters { };
/*/// ==================================================================================================================================
	So a grammar becomes a set of productions. This implementation simply uses a vector where the ith element holds the production
	whose number is i. We simply expand the vector when we add a production whose number is outside its limit.
	
	UR for user registers
/*/// ----------------------------------------------------------------------------------------------------------------------------------
	JIG_PUBLIC template <typename S, typename UR = nilregisters> class grammar {
	JIG_PUBLISHED:
		typedef grammar <S, UR> type;
		typedef typename matched <typename S::value_t>::type string_t;
		typedef regpack <string_t, UR> regs_t;
		typedef S stream_t;
		
		grammar () { }
		
		~grammar () { }
		
		template <typename G> void add (size_t production_num, const G& goal) {
			auto prod = new production<type, G> (production_num, goal);
			if (prods.size() <= production_num) {
				prods.resize (production_num + 1);
				memos.resize (production_num + 1); // one memo for every production
			}
			
			prods [production_num] = prod;
			memos [production_num] = 0;
		}
		
		bool parse (size_t production_num, S& s) {
#ifdef JIG_LOGGING
			grammar& g = *this;
#endif
			LOG_RULE(production_num)
			if (prods[production_num]) {
				if ((*prods[production_num])(s, *this, false)) { LOG_SUCCESS return true; }
				LOG_FAIL return false;
			}
			LOG_FAIL
			return false;
		}
		
		void apply_actions () {
			escape = false;
			for (auto i = toapply.begin(); i != toapply.end(); ++i) {
				(*i).second ((*i).first, escape);
				if (escape) break;
			}
			
			toapply .clear ();
		}
		
	public:
		// no copying or moving. in the presense of actions bad things could happen
		grammar (grammar&& g) = delete;
		grammar& operator=(grammar&& g) = delete;
		grammar (const grammar& g) = delete;
		grammar& operator=(const grammar& g) = delete;

		const string_t& matched () const { return state.text; }
		
		size_t& get_memo (size_t which) {
			return memos[which];
		}
		
		void begin_saving_items (S& s) { markbeg = s.tell(); }
		
		void end_saving_items (S& s) {
			size_t n = s.tell();
			s.seek(markbeg);
			state.text.clear();
			for (size_t i = markbeg; i != n; ++i) { state.text.push_back (s.value()); s.advance(); }
		}
		
		size_t action_queue_state ()               { return toapply.size(); }
		void set_action_queue_state (size_t state) { toapply .resize (state); }

		void queue_action (std::function<void(regs_t&, bool&)>& anaction) {
			toapply .push_back (actiontodo (state, anaction));
		}
		
		void reset () {
			for (auto i = memos.begin(); i != memos.end(); ++i) *i = 0;
		}
		
		regs_t& get_state() { return state; }
		
#ifdef JIG_LOGGING
		void set_logger (logger<typename S::value_t>& alogger) { log = &alogger; }
		logger<typename S::value_t>* logger() { return log; }
#endif
	private:
		typedef abproduction<grammar<S, UR>> production_t;
		typedef std::pair <regs_t, std::function<void(regs_t&, bool&)>> actiontodo;

		std::vector <production_t*> prods;
		std::vector <size_t>        memos;
		regs_t    state;
		size_t    markbeg;
		bool      escape;
		std::vector <actiontodo>    toapply;

#ifdef JIG_LOGGING
		jig::logger <typename S::value_t>* log;
#endif
	};

} // end of jig namespace

#undef JIG_PUBLISHED
#undef JIG_PUBLIC
#undef JIG_INTERNAL
#endif
