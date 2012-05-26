function [s] = objectminusgreaterThanplString(o)
%
s = funcall(@lambdaminus60159, class(o));
function [retvalminus60160] = lambdaminus60159(caseValueminus60158)
%
retvalminus60160 = funcall(condHelperFirstTrue(cellArray(cellArray(@lambdaminus60161, @lambdaminus60163), cellArray(@lambdaminus60165, @lambdaminus60167))));
function [retvalminus60168] = lambdaminus60167()
%
retvalminus60168 = stringminusgreaterThanrstring(o);

end
function [retvalminus60166] = lambdaminus60165()
%
retvalminus60166 = equalOneOf(caseValueminus60158, cellArray('char'));

end
function [retvalminus60164] = lambdaminus60163()
%
retvalminus60164 = numericminusgreaterThanrstring(o);

end
function [retvalminus60162] = lambdaminus60161()
%
retvalminus60162 = equalOneOf(caseValueminus60158, cellArray('double', 'logical', 'integer'));

end

end

end