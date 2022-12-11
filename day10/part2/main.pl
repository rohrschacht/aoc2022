use strict;
use warnings FATAL => 'all';

sub signal_strength {
    my ($states_reference, $index) = @_;
    return $index * $states_reference->[$index - 1];
}

sub sprite_visible {
    my ($sprite_position, $beam_position) = @_;
    return abs($beam_position - $sprite_position) <= 1;
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

for my $row (0..5) {
    for my $column (0..39) {
        my $cycle = (40 * $row) + $column;
        my $current_x = $X_states[$cycle];
        if (sprite_visible($current_x, $column)) {
            print("#");
        } else {
            print(".");
        }
    }
    print("\n");
}
