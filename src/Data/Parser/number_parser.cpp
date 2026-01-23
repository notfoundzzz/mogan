
/******************************************************************************
 * MODULE     : number_parser.cpp
 * DESCRIPTION: shared number parsing routines for various programming languages
 * COPYRIGHT  : (C) 2019  Darcy Shen
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "number_parser.hpp"
#include "analyze.hpp"

number_parser_rep::number_parser_rep ()
    : PREFIX_0B ("prefix_0b"), PREFIX_0O ("prefix_0o"), PREFIX_0X ("prefix_0x"),
      PREFIX_HASH ("prefix_#"), NO_SUFFIX_WITH_BOX ("no_suffix_with_box"),
      SCIENTIFIC_NOTATION ("sci_notation") {
  separator            = '\0';
  check_path_boundaries= false;
}

void
number_parser_rep::parse_binary (string s, int& pos) {
  while (pos < N (s)) {
    if (is_binary_digit (s[pos])) {
      pos++;
    }
    else if (is_separator (s[pos])) {
      // Separator handling depends on path boundary checking
      if (check_path_boundaries) {
        // With path boundary checking: separator must be followed by a binary
        // digit
        if (pos + 1 < N (s) && is_binary_digit (s[pos + 1])) {
          pos++;
        }
        else {
          break;
        }
      }
      else {
        // Without path boundary checking: allow separator as before
        pos++;
      }
    }
    else {
      break;
    }
  }
}

void
number_parser_rep::parse_octal (string s, int& pos) {
  while (pos < N (s)) {
    if (is_octal_digit (s[pos])) {
      pos++;
    }
    else if (is_separator (s[pos])) {
      // Separator handling depends on path boundary checking
      if (check_path_boundaries) {
        // With path boundary checking: separator must be followed by an octal
        // digit
        if (pos + 1 < N (s) && is_octal_digit (s[pos + 1])) {
          pos++;
        }
        else {
          break;
        }
      }
      else {
        // Without path boundary checking: allow separator as before
        pos++;
      }
    }
    else {
      break;
    }
  }
}

void
number_parser_rep::parse_hex (string s, int& pos) {
  while (pos < N (s)) {
    if (is_hex_digit (s[pos])) {
      pos++;
    }
    else if (is_separator (s[pos])) {
      // Separator handling depends on path boundary checking
      if (check_path_boundaries) {
        // With path boundary checking: separator must be followed by a hex
        // digit
        if (pos + 1 < N (s) && is_hex_digit (s[pos + 1])) {
          pos++;
        }
        else {
          break;
        }
      }
      else {
        // Without path boundary checking: allow separator as before
        pos++;
      }
    }
    else {
      break;
    }
  }
}

void
number_parser_rep::parse_decimal (string s, int& pos) {
  while (pos < N (s)) {
    if (is_digit (s[pos])) {
      pos++;
    }
    else if (is_separator (s[pos])) {
      // Separator handling depends on path boundary checking
      if (check_path_boundaries) {
        // With path boundary checking: separator must be followed by a digit
        if (pos + 1 < N (s) && is_digit (s[pos + 1])) {
          pos++;
        }
        else {
          break;
        }
      }
      else {
        // Without path boundary checking: allow separator as before
        pos++;
      }
    }
    else if (s[pos] == '.') {
      // Decimal point handling depends on path boundary checking
      if (check_path_boundaries) {
        // With path boundary checking: decimal point must be followed by a
        // digit
        if (pos + 1 < N (s) && is_digit (s[pos + 1])) {
          pos++;
        }
        else {
          break;
        }
      }
      else {
        // Without path boundary checking: allow decimal point as before
        pos++;
      }
    }
    else {
      break;
    }
  }
}

bool
number_parser_rep::can_parse_prefix_b (string s, int pos) {
  return pos + 2 < N (s) && (s[pos + 1] == 'b' || s[pos + 1] == 'B') &&
         ((prefix_0b () && s[pos] == '0') || (prefix_hash () && s[pos] == '#'));
}

bool
number_parser_rep::can_parse_prefix_o (string s, int pos) {
  return pos + 2 < N (s) && (s[pos + 1] == 'o' || s[pos + 1] == 'O') &&
         ((prefix_0o () && s[pos] == '0') || (prefix_hash () && s[pos] == '#'));
}

bool
number_parser_rep::can_parse_prefix_x (string s, int pos) {
  return pos + 2 < N (s) && (s[pos + 1] == 'x' || s[pos + 1] == 'X') &&
         ((prefix_0x () && s[pos] == '0') || (prefix_hash () && s[pos] == '#'));
}

bool
number_parser_rep::can_parse (string s, int pos) {
  // Check that the preceding character is not a word character if path boundary
  // checking is enabled
  if (check_path_boundaries && pos > 0) {
    char prev= s[pos - 1];
    if (is_alpha (prev) || is_digit (prev) || prev == '_') {
      return false;
    }
    // Additionally, avoid matching numbers after '.' or '/' (common in paths)
    // Also avoid matching after '@' (email/git user separator) and ':'
    // (URL/Windows drive separator) Also avoid matching after '_' (common in
    // filenames) For '-', only avoid matching if it's likely a path separator
    // (preceded by a word character)
    if (prev == '.' || prev == '/' || prev == '\\' || prev == '@' ||
        prev == ':' || prev == '_') {
      return false;
    }
    if (prev == '-') {
      // Check if '-' is likely a path separator (preceded by a letter or
      // underscore) If not, it might be a minus sign for negative numbers
      // Numbers before '-' are allowed for arithmetic expressions
      if (pos > 1) {
        char prev2= s[pos - 2];
        if (is_alpha (prev2) || prev2 == '_') {
          return false; // '-' is preceded by letter or underscore, likely path
                        // separator
        }
      }
      // '-' at start of string or not preceded by letter/underscore, could be
      // minus sign Don't return false, allow parsing to continue
    }
  }

  // check on len >= 3
  if (pos + 2 < N (s)) {
    if (can_parse_prefix_b (s, pos) || can_parse_prefix_x (s, pos) ||
        can_parse_prefix_o (s, pos))
      return true;
  }
  // check on len >= 2
  if (pos + 1 < N (s)) {
    if (s[pos] == '.' && is_digit (s[pos + 1])) return true;
  }
  // finally, check on len >= 1
  return pos < N (s) && is_digit (s[pos]);
}

void
number_parser_rep::do_parse (string s, int& pos) {
  int orig_pos= pos;
  if (pos >= N (s)) return;

  if (!is_digit (s[pos]) && !(prefix_hash () && s[pos] == '#') &&
      !(s[pos] == '.' && pos + 1 < N (s) && is_digit (s[pos + 1])))
    return;

  // for #t and #f
  if (prefix_hash () && pos + 1 < N (s) && s[pos] == '#' &&
      (s[pos + 1] == 't' || s[pos + 1] == 'f')) {
    pos+= 2;
    return;
  }

  // Start with 0b, 0o, 0x, #b, #o, #x
  if (can_parse_prefix_b (s, pos)) {
    pos+= 2;
    parse_binary (s, pos);
    if (no_suffix_with_box ()) {
      // Check path boundaries if enabled
      if (check_path_boundaries && pos < N (s)) {
        char next= s[pos];
        // Check if next character is a path separator
        if (next == '.' || next == '/' || next == '\\' || next == '@' ||
            next == ':' || next == '_') {
          pos= orig_pos;
        }
        else if (next == '-') {
          // Check if '-' is likely a path separator (followed by a letter or
          // underscore) If followed by letter or underscore, it might be a path
          // separator Numbers after '-' are allowed for negative numbers and
          // arithmetic expressions
          if (pos + 1 < N (s)) {
            char next2= s[pos + 1];
            if (is_alpha (next2) || next2 == '_') {
              // '-' followed by letter or underscore, likely path separator
              pos= orig_pos;
            }
            // Otherwise, '-' might be a minus sign in arithmetic expression or
            // part of a number
          }
        }
      }
      return;
    }
  }
  if (can_parse_prefix_o (s, pos)) {
    pos+= 2;
    parse_octal (s, pos);
    if (no_suffix_with_box ()) {
      // Check path boundaries if enabled
      if (check_path_boundaries && pos < N (s)) {
        char next= s[pos];
        // Check if next character is a path separator
        if (next == '.' || next == '/' || next == '\\' || next == '@' ||
            next == ':' || next == '_') {
          pos= orig_pos;
        }
        else if (next == '-') {
          // Check if '-' is likely a path separator (followed by a letter or
          // underscore) If followed by letter or underscore, it might be a path
          // separator Numbers after '-' are allowed for negative numbers and
          // arithmetic expressions
          if (pos + 1 < N (s)) {
            char next2= s[pos + 1];
            if (is_alpha (next2) || next2 == '_') {
              // '-' followed by letter or underscore, likely path separator
              pos= orig_pos;
            }
            // Otherwise, '-' might be a minus sign in arithmetic expression or
            // part of a number
          }
        }
      }
      return;
    }
  }
  if (can_parse_prefix_x (s, pos)) {
    pos+= 2;
    parse_hex (s, pos);
    if (no_suffix_with_box ()) {
      // Check path boundaries if enabled
      if (check_path_boundaries && pos < N (s)) {
        char next= s[pos];
        // Check if next character is a path separator
        if (next == '.' || next == '/' || next == '\\' || next == '@' ||
            next == ':' || next == '_') {
          pos= orig_pos;
        }
        else if (next == '-') {
          // Check if '-' is likely a path separator (followed by a letter or
          // underscore) If followed by letter or underscore, it might be a path
          // separator Numbers after '-' are allowed for negative numbers and
          // arithmetic expressions
          if (pos + 1 < N (s)) {
            char next2= s[pos + 1];
            if (is_alpha (next2) || next2 == '_') {
              // '-' followed by letter or underscore, likely path separator
              pos= orig_pos;
            }
            // Otherwise, '-' might be a minus sign in arithmetic expression or
            // part of a number
          }
        }
      }
      return;
    }
  }

  parse_decimal (s, pos);
  if (scientific_notation () && pos < N (s) &&
      (s[pos] == 'e' || s[pos] == 'E')) {
    pos++;
    if (pos < N (s) && s[pos] == '-') pos++;
    parse_decimal (s, pos);
  }
  suffix_parser.parse (s, pos);

  // Check path boundaries if enabled
  if (check_path_boundaries && pos < N (s)) {
    char next= s[pos];
    // Check if next character is a path separator
    if (next == '.' || next == '/' || next == '\\' || next == '@' ||
        next == ':' || next == '_') {
      pos= orig_pos;
    }
    else if (next == '-') {
      // Check if '-' is likely a path separator (followed by a letter or
      // underscore) If followed by letter or underscore, it might be a path
      // separator Numbers after '-' are allowed for negative numbers and
      // arithmetic expressions
      if (pos + 1 < N (s)) {
        char next2= s[pos + 1];
        if (is_alpha (next2) || next2 == '_') {
          // '-' followed by letter or underscore, likely path separator
          pos= orig_pos;
        }
        // Otherwise, '-' might be a minus sign in arithmetic expression or part
        // of a number
      }
    }
  }
}

void
number_parser_rep::use_fortran_style () {
  support_scientific_notation (true);
}

void
number_parser_rep::use_r_style () {
  // support_long_suffix (true);
  // support_locase_i_suffix (true);
  support_scientific_notation (true);
  support_prefix_0x (true);
}
