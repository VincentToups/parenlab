function [r] = equal_(input)
%Parse an underscore.
r = fif(strcmp('_', first(input)), @()cellArray('_', input(((2):(end)))), @()cellArray());

end