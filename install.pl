use strict;
use warnings;
use lib "./perl-modules/lib/perl5";
use Text::Table;
use POSIX qw(ceil);

sub make_pkg_list;
sub make_pkg_desc;
sub select_pkgs;
sub install_pkgs;
sub make_pkg_page ($$$$\@);
sub check_sudo {return 1 unless int `id -u`; 0;}
sub clear_screen {print "[2J";}
my (@package_list);
@package_list = make_pkg_list;

sub make_pkg_list
{
  my @pkg_list;
  open my $PKG_FILE, "< void-packages.txt"
    or die "Error opening package list file.";

  while (<$PKG_FILE>)
    {
      chomp;
      push @pkg_list, [1, $_, make_pkg_desc $_];
    }

  close $PKG_FILE;
  @pkg_list;
}

sub select_pkgs
{
  my $cur_page = 0;
  my $per_page = 10;
  my $total_pages = ceil (@_ / $per_page) - 1;
  my $flag = check_sudo;
  make_pkg_page $cur_page, $per_page, $total_pages, $flag, @_;

  while (<STDIN>)
    {
      next unless m/([a-jQIMUNP])/;
      last if $1 eq 'Q';
      $cur_page++ if $1 eq 'N' and $cur_page < $total_pages;
      $cur_page-- if $1 eq 'P' and $cur_page > 0;
      install_pkgs @_ if $1 eq 'I' and $flag;
      do {$_ -> [0] = 1 for @_;} if $1 eq 'M';
      do {$_ -> [0] = undef for @_} if $1 eq 'U';
      my $choice = $cur_page * $per_page + ord ($1) - 97;
      $_[$choice] -> [0] = (not $_[$choice] -> [0])
        if $_[$choice] and $1 =~ m/[a-j]/;
      make_pkg_page $cur_page, $per_page, $total_pages, $flag, @_;
    }
}

sub install_pkgs
{
  my @pkgs_to_install;
  $_ -> [0] and push (@pkgs_to_install, $_ -> [1]) for @_;
  system "xbps-install", "-Sf", @pkgs_to_install;
  sleep 5;
}

sub make_pkg_page ($$$$\@)
{
  clear_screen;
  my ($cur_page, $per_page, $total_pages, $flag, $array, @local_array) = @_;
  my $table = Text::Table -> new (qw(Key Install Name Description));
  my $page_start = $cur_page * $per_page;
  @local_array = @$array[$page_start .. $page_start + $per_page - 1]
    if $cur_page < $total_pages;
  @local_array = @$array[$page_start .. $#$array]
    if $cur_page == $total_pages;
  $local_array[$_] = [chr ($_ + 97), @{$local_array[$_]}]
    for (0 .. $#local_array);
  $table -> load (@local_array);
  print "$table\n==========\n";
  print "***YOU MUST RUN THIS PROGRAM AS ROOT TO INSTALL PACKAGES!***\n\n"
    unless $flag;
  print <<EOF;
<a-j> to select packages to install
<M> to mark all packages, <U> to unmark all packages
<N> to go to the next page, <P> to go to the previous page
<I> to install selected packages, <Q> to quit
EOF
  print "\n";
}

sub make_pkg_desc
{
  my $desc = `xbps-query -S $_[0] --property short_desc`;
  chomp $desc;
  $desc;
}

select_pkgs make_pkg_list;
