String.prototype.is_in_language = function(lang) {
    var letters = this.split(''),
        letters_array_copy = letters.slice(0),
        total_letters = letters.length;
        max_allowed_number_of_letters_in_series = 0;

    // find max number of letters allowed in series
    var last_letter = null;
    while (letters_array_copy.length > 0) {
        var letter = letters_array_copy.shift();
        if (last_letter !== null && letter !== last_letter) {
            break;
        }
        last_letter = letter;
        max_allowed_number_of_letters_in_series++;
    }

    // check letter sequence if it exceeds max_allowed_number_of_letters_in_series
    var current_letter_count = 0;
    last_letter = null;
    letter = null;

    for (var i = 0; i < total_letters; i++) {
        var letter = letters[i];
        if (last_letter === letter) {
            current_letter_count++;
        }
        else {
            current_letter_count = 1;
        }

        if (current_letter_count > max_allowed_number_of_letters_in_series) {
            return false;
        }
        last_letter = letter;
    };

    return true;
};


var inputs = {
    '1122': 'a^nb^n',
    'aabb': 'a^nb^n',
    'abb': 'a^nb^nc^n',
    'aabbcc': 'a^nb^nc^n',
};

Object.keys(inputs).forEach(function(word){
    var lang = inputs[word];
    print_result(word, lang, word.is_in_language(lang));
});

function print_result(word, lang, contained) {
    console.log(word + ' in ' + lang + ': ' + (contained ? 'yes' : 'no'));
}