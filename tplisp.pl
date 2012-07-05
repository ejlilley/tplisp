#!/usr/bin/perl

# package Lisp::Tiny;
package L;
use strict;
use warnings;
no warnings 'recursion';
use Scalar::Util qw(looks_like_number);
use Data::Dumper;
use Data::Dump::Streamer;
use List::Util qw(reduce);
use Getopt::Long;
use Sub::Call::Recur;
use Sub::Call::Tail;
use Carp qw(confess carp croak cluck);
use Memoize;
our ($a, $b);

my %options = ( );

GetOptions(\%options, 'load=s', 'norepl!');

sub car($) {
    my ($exp) = @_;
    return $exp->[0];
}

sub cdr($) {
    my ($exp) = @_;
    return $exp->[1];
}

sub rest($) {
    my ($exp) = @_;
    return [@$exp[1..$#$exp]];
}

sub array2ll($);
sub array2ll($) {
    my ($array) = @_;
    if (! ref($array) || ! (ref($array) eq 'ARRAY')) {
	return $array;
    } elsif (scalar @$array == 0) {
	return [];
    } elsif (scalar @$array == 1) {
	return [array2ll($array->[0]),[]];
    } else {
	return [array2ll($array->[0]),array2ll(rest($array))];
    }
}

sub ll2array($);
sub ll2array($) {
    my ($ll) = @_;
    if (! ref($ll) || ! (ref($ll) eq 'ARRAY')) {
	return $ll;
    } elsif (scalar @{cdr($ll)} == 0) {
	return [ll2array(car($ll))];
    } else {
	return [ll2array(car($ll)), @{ll2array(cdr($ll))}];
    }
}

sub ll2array1($); # only transform top level
sub ll2array1($) {
    my ($ll) = @_;
    if (! ref($ll) || ! (ref($ll) eq 'ARRAY')) {
	return $ll;
    } elsif (scalar @{cdr($ll)} == 0) {
	return [car($ll)];
    } else {
	return [car($ll), @{ll2array1(cdr($ll))}];
    }
}


sub llmap (&$);
sub llmap (&$) {
    my ($proc,$ll) = @_;
    local $_ = car($ll);
    if (scalar @$ll == 0) {
	return [];
    } elsif (scalar @{cdr($ll)} == 0) {
	return [$proc->(car($ll)), []];
    } else {
	return [$proc->(car($ll)), llmap {$proc->($_[0])} cdr($ll)];
    }
}

# *handle_error = \&confess;
sub handle_error {
    die "Error at line $.! @_\n"; # exit from surrounding eval{}
}

sub handle_warning {
    warn "Warning at line $.! @_\n";
}

sub return_parse_tree ($) {
    # parse() returns ordinary arrays -- tpeval requires lisp-style
    # linked lists
    return [map {array2ll $_} (@{parse(tokenize($_[0]),[])}) ];
}

sub tokenize ($) {
    my ($input) = @_;
    # better tokenizer, based on HOP p. 366
    my $tokens = sub ($) {
	my $target = shift;
	return sub {
	  TOKEN: { return ['STRING', $1]     if $target =~ /\G "([^"]*)"      /gcx;
		   return ['QUOTE']          if $target =~ /\G '              /gcx;
		   return ['DOT']            if $target =~ /\G \.             /gcx;
		   return ['VECTOR']         if $target =~ /\G \#(?=\()       /gcx;
		   return ['OPENPAREN']      if $target =~ /\G \(             /gcx;
		   return ['CLOSEPAREN']     if $target =~ /\G \)             /gcx;
		   return ['SYMBOL', $1]     if $target =~ /\G (\#[a-z])      /gcx;
		   return ['SYMBOL', $1]     if $target =~ /\G ([^\(\)\s"']+) /gcx;
		   redo TOKEN                if $target =~ /\G \s+            /gcx;
		   redo TOKEN                if $target =~ /\G \n+            /gcx;
		   return ['UNKNOWN', $1]    if $target =~ /\G (.)            /gcx;
		   return;
	    }
	};
    };
    my $return_tokens = $tokens->($input);
    my @result;
    while (my $token = $return_tokens->()) {
	push @result, $token;
    }
    return \@result;
}

sub parse($;$);
sub parse($;$) {
    my ($unparsed,$parsed) = @_;
    my $token = $unparsed->[0];
    my $nexttoken = $unparsed->[1];
    unless (scalar @$unparsed) {
	return ([],$parsed);
    } elsif ($token->[0] eq "QUOTE") {
	my ($new_unparsed,$new_parsed) = parse(rest($unparsed),[]);
	return ($new_unparsed,[@$parsed, ['quote', $new_parsed->[0]], @{rest($new_parsed)}]);
    } elsif ($token->[0] eq "VECTOR") {
	my ($new_unparsed,$new_parsed) = parse(rest($unparsed),[]);
	return ($new_unparsed,[@$parsed, ['vector', @{$new_parsed->[0]}], @{rest($new_parsed)}]);
    } elsif ($token->[0] eq "OPENPAREN") {
	my ($new_unparsed,$new_parsed) = parse(rest($unparsed),[]);
	@_ = ($new_unparsed,[@$parsed, $new_parsed]); goto &parse;
# 	tail parse($new_unparsed,[@$parsed, $new_parsed]);
    } elsif ($token->[0] eq "CLOSEPAREN") {
	return (rest($unparsed),$parsed);
    } elsif ($token->[0] eq "SYMBOL" || $token->[0] eq "STRING") {
	@_ = (rest($unparsed),[@$parsed,atom($token)]); goto &parse;
# 	tail parse(rest($unparsed),[@$parsed,atom($token)]);
    } else {
	handle_error("Don't understand unknown token $token->[1]");
	return;
    }
}

sub atom($) {
    my ($token) = @_;
    if (looks_like_number("$token->[1]")) {
	return \ $token->[1];
#     } elsif ($token->[1] =~ /^(#t|#f)$/) {
# 	return $token->[1];
    } elsif ($token->[0] eq 'STRING') {
	return \ $token->[1];
    } else {
	return $token->[1];
    }
}

sub print_ll($);
sub print_ll($) {
    my ($symbol) = @_;
    if (ref $symbol eq 'ARRAY') {
	if ((ref $symbol->[1] eq 'ARRAY') && (scalar @{$symbol->[1]} == 0)) {
	    # return (to_string($symbol->[0]));
	    return (stringify($symbol->[0]));
	};
	if (scalar @$symbol == 2 && ! (ref $symbol->[1] eq 'ARRAY')) {
	    # return (to_string($symbol->[0]) . " . " . (to_string($symbol->[1])));
	    return (stringify($symbol->[0]) . " . " . (stringify($symbol->[1])));
	}
	# return (to_string($symbol->[0]),print_ll($symbol->[1]));
	return (stringify($symbol->[0]),print_ll($symbol->[1]));
    } else {
	# return to_string($symbol);
	return stringify($symbol);
    }
}

sub to_string($;$);
sub to_string($;$) {
    my ($symbol,$type) = @_;
    if (ref $symbol eq 'ARRAY') {
	if (scalar @$symbol == 0) {
	    return "()";
	} elsif (scalar @$symbol == 2) {
	    return ("(" . join(" ", print_ll($symbol)) . ")");
	} else {
	    return ("#(" . join(" ", map {stringify($_)} (@$symbol)) . ")");
	}
    } elsif (ref $symbol eq 'SCALAR') {
	return $$symbol if (looks_like_number($$symbol));
	return "\"" . $$symbol . "\"";
    } elsif (ref $symbol eq 'HASH') {
	return "<" . $type->{e} . ":" . stringify($symbol->{e}) . ">";
    } else {
	return $symbol;
    }
}

sub find_type($$);
sub find_type($$) {
    my ($env, $type) = @_;
    if (exists v($env)->{$type}) {
	return v($env)->{$type};
    } elsif (exists v($env)->{"!!outer"}) {
	@_ = (v($env)->{"!!outer"},$type); goto &find_type;
# 	tail find_type(v($env)->{"!!outer"},$type);
    } else {
	handle_error("Couldn't find type $type");
	return;
    }
}

sub find_env($$);
sub find_env($$) {
    my ($env, $var) = @_;
    if (exists v($env)->{$var}) {
	return $env;
    } elsif (exists v($env)->{"!!outer"}) {
	@_ = (v($env)->{"!!outer"},$var); goto &find_env;
# 	tail find_env(v($env)->{"!!outer"},$var);
    } else {
	handle_error("Couldn't find variable $var");
    }
}

sub load_file ($$) {
    my ($filename,$env) = @_;
    open(my $fh, "<", $filename);
    local $/ = undef;
    my $file = <$fh>;
    $file =~ s/\;[^\n]*/ /gm;
    $file =~ s/\n+/ /gm;
    my @val;
    eval { @val = map {tpeval($_,$env)} (@{return_parse_tree($file)}); };
    if ($@) {
	handle_error($@);
    }
}

sub add_globals($) {
    my ($env) = @_;
    $env->{t} = 'env' if (! $env->{t});
    $env->{v} = {} if ((! $env->{v}) || (ref($env->{v}) ne 'HASH'));
    my $e = $env->{v};
    $e->{"+"} = { t => 'proc',
		  v => sub {\ reduce {$a + $b} (map {v($_)} (@{ll2array($_[0])}))},
		  e => '+',
		  d => "Add numbers together.",};
    $e->{"-"} = { t => 'proc',
		  v => sub {\ (defined car(cdr($_[0])) ? ${car($_[0])} - ${car(cdr($_[0]))} : 0 - ${car($_[0])})},
		  e => '-',
		  d => "Subtract numers from each other."};
    $e->{"*"} = { t => 'proc',
		  v => sub {\ reduce {$a * $b} (map {$$_} (@{ll2array($_[0])}))},
		  e => '*',
		  d => "Multiply numbers with each other."};
    $e->{"/"} = { t => 'proc',
		  v => sub {\ (${car($_[0])} / ${car(cdr($_[0]))})},
		  e => '/',
		  d => "Divide numbers by each other."};
    $e->{"%"} = { t => 'proc',
		  v => sub {\ (${car($_[0])} % ${car(cdr($_[0]))})},
		  e => '%'};
    $e->{">"} = { t => 'proc',
		  v => sub {(${car($_[0])} > ${car(cdr($_[0]))}) && $e->{"#t"} || $e->{"#f"}},
		  e => '>'};
    $e->{"<"} = { t => 'proc',
		  v => sub {(${car($_[0])} < ${car(cdr($_[0]))}) && $e->{"#t"} || $e->{"#f"}},
		  e => '<'};
    $e->{">="} = { t => 'proc',
		   v => sub {(${car($_[0])} >= ${car(cdr($_[0]))}) && $e->{"#t"} || $e->{"#f"}},
		   e => '>='};
    $e->{"<="} = { t => 'proc',
		   v => sub {(${car($_[0])} <= ${car(cdr($_[0]))}) && $e->{"#t"} || $e->{"#f"}},
		   e => '<='};
    $e->{"="} = { t => 'proc',
		  v => sub {(${car($_[0])} == ${car(cdr($_[0]))}) && $e->{"#t"} || $e->{"#f"}},
		  e => '='};
    $e->{"eq?"} = { t => 'proc',
		    v => sub {(car($_[0]) eq car(cdr($_[0]))) && $e->{"#t"} || $e->{"#f"}},
		    e => 'eq?'};
    $e->{"perl"} = { t => 'proc',
		     v => sub {eval "${car($_[0])}"},
		     e => 'perl'};
    $e->{"dump"} = { t => 'proc',
		     v => sub {print Dumper car($_[0])},
		     e => 'dump'};
    $e->{"#t"} = { t => 'bool',
		   e => '#t',
		   v => 1,};
    $e->{"#f"} = { t => 'bool',
		   e => '#f',
		   v => 0,};
    $e->{"and"} = { t => 'proc',
		    e => 'and',
		    v => sub { ((v(car($_[0])) && v(car(cdr($_[0])))) ? $e->{"#t"} : $e->{"#f"}) } };
    $e->{"or"} = { t => 'proc',
		    e => 'or',
		    v => sub { ((v(car($_[0])) || v(car(cdr($_[0])))) ? $e->{"#t"} : $e->{"#f"}) },};
    $e->{"nil"} = { t => 'lst',
		    v => [],
		    e => '()',};
    $e->{"apply"} = { t => 'proc',
		      v => sub {v(car($_[0]))->(car(cdr($_[0])))},
		      e => 'apply'};
    $e->{"car"} = { t => 'proc',
		    v => sub {car(car($_[0]))},
		    e => 'car'};
    $e->{"cdr"} = { t => 'proc',
		    v => sub {cdr(car($_[0]))},
		    e => 'cdr'};
    $e->{"cons"} = { t => 'proc',
		     v => sub {[car($_[0]), car(cdr($_[0]))]},
		     e => 'cons',};
    $e->{"identity"} = { t => 'proc',
			 v => sub {car($_[0])},
			 e => 'identity'};
    $e->{"list"} = { t => 'proc',
		     v => sub {$_[0]},
		     e => 'list'};
    $e->{"list?"} = { t => 'proc',
		      v => sub {ref(car($_[0])) eq 'ARRAY' ? $e->{"#t"} : $e->{"#f"}},
		      e => 'list?'};
    $e->{"null?"} = { t => 'proc',
		      v => sub { ((ref(car($_[0])) eq 'ARRAY') && (scalar @{car($_[0])} == 0)) ?
				     $e->{"#t"} : $e->{"#f"} },
		      e => 'null?',};
    $e->{"vector"} = { t => 'proc',
		       v => sub {{t => 'vec', v => ll2array1($_[0])}},
		       e => 'vector'};
    $e->{"vector-map"} = { t => 'proc',
			   v => sub {{t => 'vec', v => [map {v(car($_[0]))->([$_,[]])} (@{car(cdr($_[0]))->{v}})]}},
			   e => 'vector-map'};
    $e->{"rest"} = { t => 'rest',
		     v => sub {rest(car($_[0]))},
		     e => 'rest'};
    $e->{"display"} = { t => 'proc',
			v => sub {print stringify(car($_[0])); print "\n"; return},
			e => 'display'};
    $e->{"newline"} = { t => 'proc',
			v => sub {print "\n"; return},
			e => 'newline'};
    $e->{"load-file"} = { t => 'proc',
			  v => sub {load_file(${car($_[0])},car(cdr($_[0]))); return},
			  e => 'load-file'};
    $e->{"eval"} = { t => 'proc',
		     v => sub {tpeval(car($_[0]),car(cdr($_[0])))},
		     e => 'eval'};
    $e->{"null?"} = { t => 'proc',
		      v => sub {(ref car($_[0]) eq 'ARRAY' && scalar @{car($_[0])} == 0) ? $e->{"#t"} : $e->{"#f"}},
		      e => 'null?'};
    $e->{"user-initial-environment"} = { t => 'env', # just like scheme!
					 v => $env->{v},
					 e => 'user-initial-environment'};
    $e->{"doc"} = { t => 'proc',
		    v => sub {(tv(car($_[0])) eq 'proc') ? (car($_[0])->{d} ? \ car($_[0])->{d} : \ "No documentation.") : \ "Not a procedure."},
		    e => 'doc'};
    $e->{"type"} = { t => 'proc',
		     v => sub {tv(car($_[0]))},
		     e => 'type'};
    return $env;
}

sub t($$) { # return type attributes
    my ($exp,$env) = @_;
    return(find_type($env,tv($exp)) || handle_error("No type for variable " . $exp->{e}));
}

sub v($) { # return value
    my ($exp) = @_;
    if (ref($exp) eq 'SCALAR') {
	return $$exp;
    } elsif (ref($exp) eq 'ARRAY') {
	return $exp;
    } else {
	return $exp->{v};
    }
}

sub tv($) { # return type
    my ($exp) = @_;
    if (ref($exp) eq 'SCALAR') {
	return 'num' if (looks_like_number($$exp));
	return 'str';
    } elsif (ref($exp) eq 'ARRAY') {
	return 'lst';
    } elsif (ref($exp) eq 'HASH') {
	return $exp->{t};
    } else {
	return 'sym';
    }
}

sub add_types($) {
    my ($env) = @_;
    # $env->{v}->{'!!types'} = {} if ((! $env->{v}->{'!!types'}) || (ref($env->{v}->{'!!types'}) ne 'HASH'));
    $env->{t} = 'env' if (! $env->{t});
    $env->{v} = {} if ((! $env->{v}) || (ref($env->{v}) ne 'HASH'));
    my $e = $env->{v};
    $e->{proc} = { e => 'Procedure',
		   t => 'typ',
		   p => \&to_string, };
    $e->{lst} = { e => 'List',
		  t => 'typ',
		  p => \&to_string, };
    $e->{env} = { e => 'Environment',
		  t => 'typ',
		  p => \&to_string, };
    $e->{typ} = { e => 'Type',
		  t => 'typ',
		  p => \&to_string, };
    $e->{sym} = { e => 'Symbol',
		  t => 'typ',
		  p => \&to_string, };
    $e->{bool} = { e => 'Boolean',
		   t => 'typ',
		   p => sub { $_[0]->{e} }, };
    $e->{num} = { e => 'Number',
		  t => 'typ',
		  p => \&to_string, };
    $e->{str} = { e => 'String',
		  t => 'typ',
		  p => \&to_string, };
    $e->{vec} = { e => 'Vector',
		  t => 'typ',
		  p => sub {"#(" . join(" ", map {stringify($_)} (@{$_[0]->{v}})) . ")"}};
    $e->{ARRAY} = $e->{list};
    $e->{SCALAR} = $e->{str};
    return $env;
}
# my $i = 0;
sub tpeval($$);
sub tpeval($$) {
    my ($x, $env) = @_;
    # print Dumper $x;
    if (ref($x) eq 'ARRAY') { # where an ARRAY here is a lisp-esque linked-list
	if (car($x) eq 'quote') {
	    return car(cdr($x));
	} elsif (car($x) eq 'atom?') {
	    return (not(ref(tpeval(car(cdr($x)),$env)) eq 'ARRAY'));
	} elsif (car($x) eq 'current-environment') {
	    return $env;
	} elsif (car($x) eq 'cond') {
	    for my $c (@{ll2array(car(cdr($x)))}) {
		my ($p,$e) = @$c;
		if (v(tpeval($p, $env))) {
		    tail tpeval($e, $env);
		    @_ = ($e, $env); goto &tpeval;
		}
	    }
	} elsif (car($x) eq 'if') {
	    my $test = car(cdr($x));
	    my $conseq = car(cdr(cdr($x)));
	    my $alt = car(cdr(cdr(cdr($x))));
# 	    tail tpeval(((v(tpeval($test,$env))) ? $conseq : $alt),$env);
	    @_ = (((v(tpeval($test,$env))) ? $conseq : $alt),$env); goto &tpeval;
	} elsif (car($x) eq 'set!') {
	    find_env($env,car(cdr($x)))->{car(cdr($x))} = tpeval(car(cdr(cdr($x))),$env);
	} elsif (car($x) eq 'define') {
	    my $var = car(cdr($x));
	    my $exps = cdr(cdr($x));
	    if (ref $var eq 'ARRAY') {
		v($env)->{car($var)} = tpeval(['lambda', [cdr($var), $exps]],$env);
	    } else {
		v($env)->{$var} = tpeval(car($exps),$env);
	    }
	} elsif (car($x) eq 'lambda' || car($x) eq 'λ') {
	    my $vars = ll2array1(car(cdr($x)));
	    my $exps = cdr(cdr($x));
	    return { t => 'proc',
		     e => $exps,
		     a => $vars,
		     d => "User-defined procedure.",
		     l => 1, # this is a 'lambda', i.e. not a builtin procedure
		     # v => sub {
		     # 	 my $args = ll2array1($_[0]);
		     # 	 my $newenv;
		     # 	 $newenv->{v} = {map {$vars->[$_] => $args->[$_]} (0..$#$vars)};
		     # 	 $newenv->{v}->{"!!outer"} = $env;
		     # 	 my $retvals = llmap {tpeval($_[0],$newenv)} ($exps);
		     # 	 return pop @{ll2array1($retvals)};
		     # },
	    };
 	} elsif (car($x) eq 'begin') {
	    my $retvals = llmap {tpeval($_[0],$env)} (cdr($x));
	    return pop @{ll2array1($retvals)};
 	} else {
	    my $exps = llmap {tpeval($_[0],$env)} ($x); # EVAL

	    if (tv(car($exps)) eq 'proc' && car($exps)->{l}) { # if it isn't a builtin (i.e. a lambda)
		my $proc = car($exps);
		my $args = ll2array1(cdr($exps));
		my $vars = $proc->{a};
		my $newenv;
		$newenv->{v} = {map {$vars->[$_] => $args->[$_]} (0..$#$vars)};
		$newenv->{v}->{"!!outer"} = $env;

		my @to_eval = @{ll2array1($proc->{e})};

		until (scalar @to_eval == 1) { # could be improved so e.g. internal definitions work
		    tpeval(shift @to_eval, $newenv);
		}
		
		# tail tpeval(shift @to_eval, $newenv);
		@_ = (shift @to_eval, $newenv); goto &tpeval;
	    } elsif (tv(car($exps)) eq 'proc' && (! car($exps)->{l})) { # if it *is* a builtin
		return v(car($exps))->(cdr($exps));
	    } else {
		return handle_error(stringify(car($exps)) . " is not a procedure.");
	    }
	}
    } elsif (ref($x) eq 'SCALAR') {
# 	print $i++, "\n";
	return $x;
    } elsif ($x) {
	if (defined find_env($env,$x)) {
	    return v(find_env($env,$x))->{$x};
	}
    }
}

our $global_env = add_globals(add_types({}));

sub stringify ($) {
    my ($exp) = @_;
    return $$exp if (ref $exp eq 'SCALAR' && looks_like_number($$exp));
    return "\"" . $$exp . "\"" if (ref $exp eq 'SCALAR');
    return to_string($exp) if (ref $exp eq 'ARRAY');
    return t($exp,$global_env)->{p}->($exp,t($exp,$global_env)) if (ref $exp eq 'HASH');
    return $exp;
}


sub repl {
    print "tplisp> ";
    while (<>) {
	chomp;
	next unless ($_);
	my @val;
	eval { @val = map {tpeval($_,$global_env)} (@{return_parse_tree($_)});};
	if ($@) {
	    print($@,"\n");
 	    print "tplisp> ";
	} elsif (scalar @val) {
	    # print Dumper \@val;
	    print join("\n", map {stringify($_)} (@val)) . "\n";
  	    print "tplisp> ";
	    next;
	} else {
	    handle_warning("Unspecified return value");
 	    print "tplisp> ";
	}
    }
}

memoize('v');
memoize('find_env'); # unbounded memory growth, but only if you use loads of variables
# memoize('tpeval'); # Stalinesque enforcing of referential transparency (might also speed up program a bit)

if (exists($options{load})) {
    load_file($options{load},$global_env);
}

repl() unless $options{norepl};

1;
