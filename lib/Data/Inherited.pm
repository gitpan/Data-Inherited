package Data::Inherited;

# $Id: Inherited.pm 11427 2006-05-09 13:00:16Z gr $

use strict;
use warnings;

our $VERSION = '1.02';


sub every_list {
    my ($self, $list_name, $override_cache) = @_;

    our %every_cache;
    my $pkg = ref $self || $self;   # can also be called as a class method
    my $list;

    unless ($list = $override_cache ? undef : $every_cache{$list_name}{$pkg}) {
        # require NEXT;
        $list = [];
        my $call = "EVERY::LAST::$list_name";
        my @every_list = $self->$call;
        return unless scalar @every_list;
        while (my ($class, $class_list) = splice(@every_list, 0, 2)) {
            push @$list => @$class_list;
        }
        $every_cache{$list_name}{$pkg} = $list;
    }
    wantarray ? @$list : $list;
}


sub every_hash {
    my ($self, $hash_name, $override_cache) = @_;

    our %every_cache;
    my $pkg = ref $self || $self;   # can also be called as a class method
    my $hash;

    unless ($hash = $override_cache ? undef : $every_cache{$hash_name}{$pkg}) {
        # require NEXT;
        $hash = {};
        my $call = "EVERY::LAST::$hash_name";
        my @every_hash = $self->$call;
        while (my ($class, $class_hash) = splice(@every_hash, 0, 2)) {
            %$hash = (%$hash, @$class_hash);
        }
        $every_cache{$hash_name}{$pkg} = $hash;
    }
    wantarray ? %$hash : $hash;
}


sub flush_every_cache_by_key {
    my ($self, $key) = @_;
    our %every_cache;
    delete $every_cache{$key};
};


# Until an updated version of NEXT.pm is released, Data::Inherited includes a
# modified version of NEXT.pm that avoids a subtle bug when used within the
# stringification method of an overloaded object. It's included within the
# this file so as to not clutter perl's lib directory, as it's only a
# temporary measure. Moreover, NEXT.pm is a core perl module, but
# Data::Inheritance installs in site_perl, leading to potentially more
# confusion.

# make perl believe that NEXT.pm is loaded so it won't overwrite our changes
# with the original NEXT.pm if somewhere it says "use NEXT".

$INC{'NEXT.pm'} = $INC{'Data/Inherited.pm'};


package NEXT;
# $VERSION = '0.60';
use Carp;
use strict;
use overload ();

no warnings 'redefine';

sub NEXT::ELSEWHERE::ancestors
{
    my @inlist = shift;
    my @outlist = ();
    while (my $next = shift @inlist) {
        push @outlist, $next;
        no strict 'refs';
        unshift @inlist, @{"$outlist[-1]::ISA"};
    }
    return @outlist;
}

sub NEXT::ELSEWHERE::ordered_ancestors
{
    my @inlist = shift;
    my @outlist = ();
    while (my $next = shift @inlist) {
        push @outlist, $next;
        no strict 'refs';
        push @inlist, @{"$outlist[-1]::ISA"};
    }
    return sort { $a->isa($b) ? -1
                : $b->isa($a) ? +1
                :                0 } @outlist;
}

sub AUTOLOAD
{
    my ($self) = @_;
    my $caller = (caller(1))[3];
    my $wanted = $NEXT::AUTOLOAD || 'NEXT::AUTOLOAD';
    undef $NEXT::AUTOLOAD;
    my ($caller_class, $caller_method) = $caller =~ m{(.*)::(.*)}g;
    my ($wanted_class, $wanted_method) = $wanted =~ m{(.*)::(.*)}g;
    croak "Can't call $wanted from $caller"
        unless $caller_method eq $wanted_method;

    my $key = ref $self && overload::Overloaded($self)
        ? overload::StrVal($self) : $self;

    local ($NEXT::NEXT{$key,$wanted_method}, $NEXT::SEEN) =
          ($NEXT::NEXT{$key,$wanted_method}, $NEXT::SEEN);


    unless ($NEXT::NEXT{$key,$wanted_method}) {
        my @forebears =
            NEXT::ELSEWHERE::ancestors ref $self || $self,
                           $wanted_class;
        while (@forebears) {
            last if shift @forebears eq $caller_class
        }
        no strict 'refs';
        @{$NEXT::NEXT{$key,$wanted_method}} =
            map { *{"${_}::$caller_method"}{CODE}||() } @forebears
                unless $wanted_method eq 'AUTOLOAD';
        @{$NEXT::NEXT{$key,$wanted_method}} =
            map { (*{"${_}::AUTOLOAD"}{CODE}) ? "${_}::AUTOLOAD" : ()} @forebears
                unless @{$NEXT::NEXT{$key,$wanted_method}||[]};
        $NEXT::SEEN->{$key,*{$caller}{CODE}}++;
    }
    my $call_method = shift @{$NEXT::NEXT{$key,$wanted_method}};
    while ($wanted_class =~ /^NEXT\b.*\b(UNSEEN|DISTINCT)\b/
           && defined $call_method
           && $NEXT::SEEN->{$key,$call_method}++) {
        $call_method = shift @{$NEXT::NEXT{$key,$wanted_method}};
    }
    unless (defined $call_method) {
        return unless $wanted_class =~ /^NEXT:.*:ACTUAL/;
        (local $Carp::CarpLevel)++;
        croak qq(Can't locate object method "$wanted_method" ),
              qq(via package "$caller_class");
    };
    return $self->$call_method(@_[1..$#_]) if ref $call_method eq 'CODE';
    no strict 'refs';
    ($wanted_method=${$caller_class."::AUTOLOAD"}) =~ s/.*:://
        if $wanted_method eq 'AUTOLOAD';
    $$call_method = $caller_class."::NEXT::".$wanted_method;
    return $call_method->(@_);
}

no strict 'vars';
package NEXT::UNSEEN;        @ISA = 'NEXT';
package NEXT::DISTINCT;        @ISA = 'NEXT';
package NEXT::ACTUAL;        @ISA = 'NEXT';
package NEXT::ACTUAL::UNSEEN;    @ISA = 'NEXT';
package NEXT::ACTUAL::DISTINCT;    @ISA = 'NEXT';
package NEXT::UNSEEN::ACTUAL;    @ISA = 'NEXT';
package NEXT::DISTINCT::ACTUAL;    @ISA = 'NEXT';

package EVERY::LAST;        @ISA = 'EVERY';
package EVERY;            @ISA = 'NEXT';
sub AUTOLOAD
{
    my ($self) = @_;
    my $caller = (caller(1))[3];
    my $wanted = $EVERY::AUTOLOAD || 'EVERY::AUTOLOAD';
    undef $EVERY::AUTOLOAD;
    my ($wanted_class, $wanted_method) = $wanted =~ m{(.*)::(.*)}g;

    my $key = ref($self) && overload::Overloaded($self)
        ? overload::StrVal($self) : $self;

    local $NEXT::ALREADY_IN_EVERY{$key,$wanted_method} =
          $NEXT::ALREADY_IN_EVERY{$key,$wanted_method};

    return if $NEXT::ALREADY_IN_EVERY{$key,$wanted_method}++;

    my @forebears = NEXT::ELSEWHERE::ordered_ancestors ref $self || $self,
                                       $wanted_class;
    @forebears = reverse @forebears if $wanted_class =~ /\bLAST\b/;
    no strict 'refs';
    my %seen;
    my @every = map { my $sub = "${_}::$wanted_method";
                  !*{$sub}{CODE} || $seen{$sub}++ ? () : $sub
                } @forebears
                unless $wanted_method eq 'AUTOLOAD';

    my $want = wantarray;
    if (@every) {
        if ($want) {
            return map {($_, [$self->$_(@_[1..$#_])])} @every;
        }
        elsif (defined $want) {
            return { map {($_, scalar($self->$_(@_[1..$#_])))}
                     @every
                   };
        }
        else {
            $self->$_(@_[1..$#_]) for @every;
            return;
        }
    }

    @every = map { my $sub = "${_}::AUTOLOAD";
               !*{$sub}{CODE} || $seen{$sub}++ ? () : "${_}::AUTOLOAD"
             } @forebears;
    if ($want) {
        return map { $$_ = ref($self)."::EVERY::".$wanted_method;
                 ($_, [$self->$_(@_[1..$#_])]);
               } @every;
    }
    elsif (defined $want) {
        return { map { $$_ = ref($self)."::EVERY::".$wanted_method;
                   ($_, scalar($self->$_(@_[1..$#_])))
                 } @every
               };
    }
    else {
        for (@every) {
            $$_ = ref($self)."::EVERY::".$wanted_method;
            $self->$_(@_[1..$#_]);
        }
        return;
    }
}


1;

__END__

=head1 NAME

Data::Inherited - hierarchy-wide accumulation of list and hash results

=head1 VERSION

This document describes version 1.00 of C<Data::Inherited>.

=head1 SYNOPSIS

  package Foo;
  use base 'Data::Inherited';
  use constant PROPERTIES => (qw/name address/);

  package Bar;
  use base 'Foo';
  use constant PROPERTIES => (qw/age/);

  package main;
  my $bar = Bar->new;
  print "$_\n" for $bar->every_list('PROPERTIES');

prints

  name
  address
  age

=head1 DESCRIPTION

This is a mixin class. By inheriting from it you get two methods that are able
to accumulate hierarchy-wide list and hash results.

=head1 METHODS

=over 4

=item every_list(String $method_name, Bool ?$override_cache = 0)

Takes as arguments a method name (mandatory) and a boolean indicating whether
to override the cache (optional, off by default)

Causes every method in the object's hierarchy with the given name to be
invoked. The resulting list is the combined set of results from all the
methods, pushed together in top-to-bottom order (hierarchy-wise).

C<every_list()> returns a list in list context and an array reference in
scalar context.

The result is cached (per calling package) and the next time the method is
called from the same package with the same method argument, the cached result
is returned. This is to speed up method calls, because internally this module
uses L<NEXT>, which is quite slow. It is expected that C<every_list()> is used
for methods returning static lists (object defaults, static class definitions
and such).  If you want to override the caching mechanism, you can provide the
optional second argument. The result is cached in any case.

See the synopsis for an example.

=item every_hash(String $method_name, Bool ?$override_cache = 0)

Takes as arguments a method name (mandatory) and a boolean indicating whether
to override the cache (optional, off by default)

Causes every method in the object's hierarchy with the given name to be
invoked. The resulting hash is the combined set of results from all the
methods, overlaid in top-to-bottom order (hierarchy-wise).

C<every_hash()> returns a hash in list context and a hash reference in scalar
context.

The cache and the optional cache override argument work like with
C<every_list()>.

Example:

  package Person;
  use base 'Data::Inherited';

  sub new {
    my $class = shift;
    my $self = bless {}, $class;
    my %args = @_;
    %args = ($self->every_hash('DEFAULTS'), %args);
    $self->$_($args{$_}) for keys %args;
    $self;
  };

  sub DEFAULTS {
    first_name => 'John',
    last_name  => 'Smith',
  };


  package Salaryman;
  use base 'Person';

  sub DEFAULTS {
    salary => 10_000,
  }


  package LocatedSalaryman;
  use base 'Salaryman';

  # Note: no default for address, but different salary

  sub DEFAULTS {
    salary     => 20_000,
    first_name => 'Johan',
  }


  package main;
  my $p = LocatedSalaryman->new;

  # salary: 20000
  # first_name: Johan
  # last_name: Smith

=back

=head1 INCOMPATIBILITIES

This module relies on L<NEXT>.  Until an updated version of L<NEXT> is
released, L<Data::Inherited> includes a modified version of L<NEXT> that
avoids a subtle bug when used within the stringification method of an
overloaded object (patch submitted to the author). It's included within the
this file so as to not clutter perl's lib directory, as it's only a temporary
measure. Moreover, L<NEXT> is a core perl module, but L<Data::Inheritance>
installs in site_perl, leading to potentially more confusion.

We make perl believe that L<NEXT> is loaded so it won't overwrite our changes
with the original L<NEXT> if somewhere it says C<use NEXT>.

=head1 BUGS AND LIMITATIONS

No bugs have been reported.

Please report any bugs or feature requests to
C<bug-data-inherited@rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org>.

=head1 INSTALLATION

See perlmodinstall for information and options on installing Perl modules.

=head1 AVAILABILITY

The latest version of this module is available from the Comprehensive Perl
Archive Network (CPAN). Visit <http://www.perl.com/CPAN/> to find a CPAN
site near you. Or see <http://www.perl.com/CPAN/authors/id/M/MA/MARCEL/>.

=head1 AUTHOR

Marcel GrE<uuml>nauer, C<< <marcel@cpan.org> >>

=head1 COPYRIGHT AND LICENSE

Copyright 2004-2007 by Marcel GrE<uuml>nauer

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

