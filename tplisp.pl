#!/usr/bin/perl

use strict;
use warnings;
use Scalar::Util qw(looks_like_number);
use Data::Dumper;
use Data::Dump::Streamer;
use List::Util qw(reduce);
use Getopt::Long;
use Carp qw(confess carp croak cluck);
our ($a, $b);

my %options = ( );

GetOptions(\%options, 'load=s');

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
    print "Error! @_\n";
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
		   return ['VECTOR']         if $target =~ /\G \#             /gcx;
		   return ['OPENPAREN']      if $target =~ /\G \(             /gcx;
		   return ['CLOSEPAREN']     if $target =~ /\G \)             /gcx;
		   return ['SYMBOL', $1]     if $target =~ /\G ([^\(\)\s"']+) /gcx;
		   redo TOKEN                if $target =~ /\G \s+            /gcx;
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
    } elsif ($token->[0] eq "CLOSEPAREN") {
	return (rest($unparsed),$parsed);
    } elsif ($token->[0] eq "SYMBOL" || $token->[0] eq "STRING") {
	@_ = (rest($unparsed),[@$parsed,atom($token)]); goto &parse;
    } else {
	handle_error("Don't understand unknown token $token->[1]");
	return;
    }
}

sub atom($) {
    my ($token) = @_;
    if (looks_like_number("$token->[1]")) {
	return \ $token->[1];
    } elsif ($token->[1] =~ /^(#t|#f)$/) {
	return \ $token->[1];
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
	    return (to_string($symbol->[0]));
	};
	if (scalar @$symbol == 2 && ! (ref $symbol->[1] eq 'ARRAY')) {
	    return (to_string($symbol->[0]) . " . " . (to_string($symbol->[1])));
	}
	return (to_string($symbol->[0]),print_ll($symbol->[1]));
    } else {
	return to_string($symbol);
    }
}

sub to_string($);
sub to_string($) {
    my ($symbol) = @_;
    if (ref $symbol eq 'ARRAY') {
	if (scalar @$symbol == 0) {
	    return "()";
	} elsif (scalar @$symbol == 2) {
	    return ("(" . join(" ", print_ll($symbol)) . ")");
	} else { # note: printing vectors is bugged, as they can be mistaken for linked lists
	    return ("#(" . join(" ", map {to_string $_} (@$symbol)) . ")");
	}
    } elsif (ref $symbol eq 'SCALAR') {
	return $$symbol;
    } elsif (ref $symbol eq 'CODE') {
	return "<Procedure:$symbol>";
    } elsif (ref $symbol eq 'HASH') {
	return "<Environment:$symbol>";
    } else {
	return $symbol;
    }
}


sub find_env($$);
sub find_env($$) {
    my ($env, $var) = @_;
    if (exists $env->{$var}) {
	return $env;
    } elsif (exists $env->{"!!outer"}) {
	return find_env($env->{"!!outer"},$var);
    } else {
	handle_error("Couldn't find variable $var");
	return;
    }
}

sub load_file ($$) {
    my ($filename,$env) = @_;
    open(my $fh, "<", $filename);
    local $/ = undef;
    my $file = <$fh>;
    $file =~ s/\n+/ /gm;
    my @val;
    eval { @val = map {tpeval($_,$env)} (@{return_parse_tree($file)}); };
    if ($@) {
	handle_error($@);
    }
}

sub add_globals($) {
    my ($env) = @_;
    $env->{"+"} = sub {\ reduce {$a + $b} (map {$$_} (@{ll2array($_[0])}))};
    $env->{"-"} = sub {\ (defined car(cdr($_[0])) ? ${car($_[0])} - ${car(cdr($_[0]))} : 0 - ${car($_[0])})};
    $env->{"*"} = sub {\ reduce {$a * $b} (map {$$_} (@{ll2array($_[0])}))};
    $env->{"/"} = sub {\ (${car($_[0])} / ${car(cdr($_[0]))})};
    $env->{"remainder"} = sub {\ (${car($_[0])} % ${car(cdr($_[0]))})};
    $env->{">"} = sub {(${car($_[0])} > ${car(cdr($_[0]))}) && \ '#t' || \ '#f'};
    $env->{"<"} = sub {(${car($_[0])} < ${car(cdr($_[0]))}) && \ '#t' || \ '#f'};
    $env->{">="} = sub {(${car($_[0])} >= ${car(cdr($_[0]))}) && \ '#t' || \ '#f'};
    $env->{"<="} = sub {(${car($_[0])} <= ${car(cdr($_[0]))}) && \ '#t' || \ '#f'};
    $env->{"="} = sub {(${car($_[0])} == ${car(cdr($_[0]))}) && \ '#t' || \ '#f'};
    $env->{"perl"} = sub {eval "${car($_[0])}"};
    $env->{"dump"} = sub {print Dumper car($_[0])};
    $env->{"#t"} = 1;
    $env->{"#f"} = 0;
    $env->{"nil"} = [];
    $env->{"apply"} = sub {car($_[0])->(car(cdr($_[0])))};
    $env->{"car"} = sub {car(car($_[0]))};
    $env->{"cdr"} = sub {cdr(car($_[0]))};
    $env->{"cons"} = sub {[car($_[0]), car(cdr($_[0]))]};
    $env->{"identity"} = sub {car($_[0])};
    $env->{"list"} = sub {$_[0]};
    $env->{"vector"} = sub {ll2array1($_[0])};
    $env->{"vector-map"} = sub {[map {car($_[0])->([$_,[]])} (@{car(cdr($_[0]))})]};
    $env->{"rest"} = sub {rest(car($_[0]))};
    $env->{"display"} = sub {print to_string(car($_[0])); print "\n"; return};
    $env->{"newline"} = sub {print "\n"; return;};
    $env->{"loadfile"} = sub {load_file(${car($_[0])},car(cdr($_[0]))); return;};
    $env->{"tpeval"} = sub {tpeval(car($_[0]),car(cdr($_[0])));};
    $env->{"null"} = sub {(ref car($_[0]) eq 'ARRAY' && scalar @{car($_[0])} == 0) ? \ '#t' : \ '#f'};
    $env->{"user-initial-environment"} = $env; # just like scheme!
    return $env;
}

sub tpeval($$);
sub tpeval($$) {
    my ($x, $env) = @_;
    if (ref($x) eq 'ARRAY') { # where an ARRAY here is a lisp-esque linked-list
	if (car($x) eq 'quote') {
	    return car(cdr($x));
	} elsif (car($x) eq 'atom?') {
	    return (not(ref(tpeval(car(cdr($x)),$env)) eq 'ARRAY'));
	} elsif (car($x) eq 'eq?') {
	    my $exp1 = car(cdr($x)); my $exp2 = car(cdr(cdr($x)));
	    my $v1 = tpeval($exp1, $env); my $v2 = tpeval($exp2, $env);
	    return (((not (ref($v1) eq 'ARRAY')) && ($v1 eq $v2)) ? \'#t' : \'#f');
	} elsif (car($x) eq 'current-environment') {
	    return $env;
	} elsif (car($x) eq 'cond') {
	    for my $c (@{ll2array(car(cdr($x)))}) {
		my ($p,$e) = @$c;
		if (${tpeval($p, $env)} eq '#t') {
		    return tpeval($e, $env);
		}
	    }
	} elsif (car($x) eq 'null?') {
	    my $r = tpeval(car(cdr($x)),$env);
	    return (((ref($r) eq 'ARRAY') && (scalar @$r == 0)) ? \'#t' : \'#f');
	} elsif (car($x) eq 'if') {
	    my $test = car(cdr($x));
	    my $conseq = car(cdr(cdr($x)));
	    my $alt = car(cdr(cdr(cdr($x))));
	    return tpeval(((${tpeval($test,$env)} eq '#t') ? $conseq : $alt),$env);
	} elsif (car($x) eq 'set!') {
	    find_env($env,car(cdr($x)))->{car(cdr($x))} = tpeval(car(cdr(cdr($x))),$env);
	} elsif (car($x) eq 'define') {
	    my $var = car(cdr($x));
	    my $exps = cdr(cdr($x));
	    if (ref $var eq 'ARRAY') {
		$env->{car($var)} = tpeval(['lambda', [cdr($var), $exps]],$env);
	    } else {
		$env->{$var} = tpeval(car($exps),$env);
	    }
	} elsif (car($x) eq 'lambda' || car($x) eq 'λ') {
	    my $vars = ll2array1(car(cdr($x)));
	    my $exps = cdr(cdr($x));
	    return sub {
		my $args = ll2array1($_[0]);
		my $newenv = {map {$vars->[$_] => $args->[$_]} (0..$#$vars)};
		$newenv->{"!!outer"} = $env;
		my $retvals = llmap {tpeval($_[0],$newenv)} ($exps);
		return pop @{ll2array1($retvals)};
	    };
 	} elsif (car($x) eq 'begin') {
	    my $retvals = llmap {tpeval($_[0],$env)} (cdr($x));
	    return pop @{ll2array1($retvals)};
 	} else {
	    my $exps = llmap {tpeval($_[0],$env)} ($x); # EVAL
	    return car($exps)->(cdr($exps)); # APPLY
	}
    } elsif (ref($x) eq 'SCALAR') {
	return $x;
    } elsif ($x) {
	if (defined find_env($env,$x)) {
	    return find_env($env,$x)->{$x};
	}
    }

}

my $global_env = add_globals({});

sub repl {
    print "tplisp> ";
    while (<>) {
	chomp;
	next unless ($_);
	my @val;
	eval { @val = map {tpeval($_,$global_env)} (@{return_parse_tree($_)});};
	if ($@) {
	    handle_error($@);
 	    print "tplisp> ";
	} elsif (scalar @val) {
 	    print join("\n", map {to_string($_)} (@val)) . "\n";
  	    print "tplisp> ";
	    next;
	} else {
	    handle_error("Unspecified return value");
 	    print "tplisp> ";
	}
    }
}

if (exists($options{load})) {
    load_file($options{load},$global_env);
}

repl();
