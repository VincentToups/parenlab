function r=andFunction(r,varargin)
%
%

clauses = [{r} varargin];
for ci=1:length(clauses)
  clause=clauses{ci};
  r = clause();
  if ~r
    return;
  end
end
