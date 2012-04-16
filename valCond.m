function r=valCond(varargin)
for vi=1:length(varargin)
  item=varargin{vi};
  condition = item{1};
  if condition()
    todos = item(2:end);
    for ti=1:(length(todos)-1)
      todo=todos{ti};
      if ischar(todo)
        evalin('caller',todo);
      else
        todo();
      end
    end
    r = todos{end};
    if ischar(r)
      r = evalin('caller',r);
    else
      r = r();
    end
    return;
  end
end

error('valCond had no succeeding terms.')
