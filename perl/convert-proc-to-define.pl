#!/usr/bin/perl
while (<>) {
    if(/^#proc/){s/proc/define/;while(defined){chomp;print"$_\\\n";$_=<>;if(/^#endproc/){$_="\n";last}}}
    print;
}
