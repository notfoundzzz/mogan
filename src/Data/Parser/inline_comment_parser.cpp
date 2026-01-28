
/******************************************************************************
 * MODULE     : inline_comment_parser.cpp
 * DESCRIPTION: shared inline comment parsing routines
 * COPYRIGHT  : (C) 2019-2020  Darcy Shen
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "inline_comment_parser.hpp"
#include "analyze.hpp"

inline_comment_parser_rep::inline_comment_parser_rep () {
  m_starts= array<string> ();
  m_require_space_before= false;
}

void
inline_comment_parser_rep::set_starts (const array<string>& p_starts) {
  m_starts= p_starts;
}

void
inline_comment_parser_rep::set_require_space_before (bool require) {
  m_require_space_before= require;
}

bool
inline_comment_parser_rep::can_parse (string s, int pos) {
  if (pos >= N (s)) return false;
  if (N (m_starts) == 0) return false;
  if (m_require_space_before && pos > 0 && !is_space (s[pos - 1])) {
    return false;
  }

  int i= 0;
  while (i < N (m_starts)) {
    string m_start= m_starts[i];
    if (test (s, pos, m_start)) {
      return true;
    }
    i++;
  }
  return false;
}

void
inline_comment_parser_rep::do_parse (string s, int& pos) {
  pos= N (s);
}
