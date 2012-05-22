function [b] = structAccessdividec(field)
%Return a function which returns the FIELD of the struct.
b = @lambdaminus111168;
function [retvalminus111169] = lambdaminus111168(theStruct)
%
retvalminus111169 = [ theStruct.(field) ];

end

end