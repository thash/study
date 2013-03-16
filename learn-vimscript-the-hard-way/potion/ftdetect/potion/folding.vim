setlocal foldmethod=indent
setlocal foldexpr=GetPotionFold(v:lnum)

function! NextNonBlankLine(lnum)
  let numlines = line('$')
  let current = a:lnum + 1

  while current <= numlines
    if getline(current) =~? '\v\S'
      return current
    endif

    let current += 1
  endwhile

  return -2
endfunction

" define indent level based on shiftwidth settings
function! IndentLevel(lnum)
  return indent(a:lnum) / &shiftwidth
endfunction

function! GetPotionFold(lnum)
  if getline(a:linum) =~? '\v^\s*$'
    return '-1'
  endif

  let this_indent = IndentLevel(a:lnum)
  let next_incent = IndentLevel(NextNonBlankLine(a:linum))

  if next_incent == this_indent
    return this_indent
  elseif next_incent < this_indent
    return this_indent
  elseif next_incent > this_indent
    return '>' . next_incent
  endif
endfunction
