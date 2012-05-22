function [oparser] = equalgreaterThanitems(n)
%Return a parser which returns N items from the input.
oparser = @lambdaminus108376;
function [retvalminus108377] = lambdaminus108376(input)
%
retvalminus108377 = fif(lessThanequal(n, length(input)), @()cellArray(input(((1):(n))), input(((plus(n, 1)):(end)))), @()cellArray());

end

end