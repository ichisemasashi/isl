#!/usr/bin/env perl
use strict;
use warnings;
use File::Path qw(make_path);

my ($input_path, $out_dir, $boundary) = @ARGV;
die "usage: multipart_extract.pl INPUT OUTDIR BOUNDARY\n"
  unless defined $input_path && defined $out_dir && defined $boundary;

make_path($out_dir) unless -d $out_dir;

open my $in, '<:raw', $input_path or die "open input: $!";
local $/;
my $raw = <$in>;
close $in;

my $delimiter = '--' . $boundary;
my @parts = split(/\Q$delimiter\E/, $raw);
my $saw_file = 0;

for my $part (@parts) {
  next if !defined $part || $part eq '' || $part =~ /^\s*--\s*$/s;
  $part =~ s/^\r?\n//;
  $part =~ s/\r?\n$//;
  next if $part eq '' || $part eq '--';

  my ($headers_raw, $body) = split(/\r?\n\r?\n/, $part, 2);
  next unless defined $headers_raw && defined $body;
  $body =~ s/\r?\n$//;

  my %headers;
  for my $line (split(/\r?\n/, $headers_raw)) {
    my ($k, $v) = split(/:\s*/, $line, 2);
    next unless defined $k && defined $v;
    $headers{lc $k} = $v;
  }

  my $cd = $headers{'content-disposition'} // '';
  next unless $cd =~ /form-data/i;

  my ($name) = $cd =~ /name="([^"]*)"/;
  next unless defined $name && length $name;

  if ($cd =~ /filename="([^"]*)"/) {
    my $filename = $1;
    next if $saw_file;
    open my $fout, '>:raw', "$out_dir/upload.bin" or die "open upload.bin: $!";
    print {$fout} $body;
    close $fout;

    open my $nout, '>:raw', "$out_dir/upload_filename.txt" or die "open upload_filename.txt: $!";
    print {$nout} $filename;
    close $nout;

    open my $tout, '>:raw', "$out_dir/upload_content_type.txt" or die "open upload_content_type.txt: $!";
    print {$tout} ($headers{'content-type'} // '');
    close $tout;
    $saw_file = 1;
    next;
  }

  my $safe = $name;
  $safe =~ s/[^A-Za-z0-9_]+/_/g;
  open my $out, '>:raw', "$out_dir/field-$safe.txt" or die "open field file: $!";
  print {$out} $body;
  close $out;
}

exit 0;
