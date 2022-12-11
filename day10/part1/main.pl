use strict;
use warnings FATAL => 'all';

sub signal_strength {
    my ($states_reference, $index) = @_;
    return $index * $states_reference->[$index - 1];
}

my $X = 1;
my @X_states = ();
open(my $input_file,  "<",  "input.txt")  or die "Can't open input.txt: $!";

while (my $line = <$input_file>) {
    if ($line =~ /noop/) {
        push(@X_states, $X);
    }
    if ($line =~ /addx/) {
        push(@X_states, $X);
        push(@X_states, $X);
        my $operand = (split ' ', $line)[1];
        $X += $operand;
    }
}

my $sum = 0;
for my $index (20, 60, 100, 140, 180, 220) {
    $sum += signal_strength(\@X_states, $index);
}
print($sum);
