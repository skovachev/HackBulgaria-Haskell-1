var inputs = [
    ['1 1', 'Vetta Tess Lejetta'],
    ['1 0', 'Jass Julietta Frass Qetta'],
];

function factorial(n, f) {
    f = f || [];
    if (n === 0 || n === 1)
        return 1;
    if (f[n] > 0)
        return f[n];
    return factorial(n - 1, f) * n;
}

function is_male_name(name) {
    var suffix = 'ss';
    return name.indexOf(suffix, name.length - suffix.length) !== -1;
}

function format_result(input, result) {
    console.log(input);
    ['male', 'female'].forEach(function(gender) {
        var count = result['unknown_' + gender + '_names_count'],
            chance_to_guess_correctly = 1;
        if (count > 1) {
            chance_to_guess_correctly = 1 / factorial(count);
        }
        // it calculates the chance to 
        console.log('Chance to guess all ' + gender + ' names: ' + (chance_to_guess_correctly * 100).toFixed(0) + '%');
    });
    console.log(''); // new line
}

inputs.forEach(function(input) {
    var o = {},
        names = input[1].split(' '),
        names_known = input[0].split(' ');

    o.male_names_known = parseInt(names_known[0], 10);
    o.female_names_known = parseInt(names_known[1], 10);
    o.unknown_male_names_count = 0;
    o.unknown_female_names_count = 0;

    while (names.length > 0) {
        var name = names.shift(),
            male_name = is_male_name(name);
        var known_names_counter = (male_name ? '' : 'fe') + 'male_names_known';
        var unknown_names_counter = 'unknown_' + (male_name ? '' : 'fe') + 'male_names_count';

        if (o[known_names_counter] > 0) {
            o[known_names_counter]--;
        } else {
            o[unknown_names_counter]++;
        }
    }

    format_result(input, o);
});