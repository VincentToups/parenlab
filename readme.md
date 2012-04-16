Parenlab
========

(Note: parenlab depends on [shadchen-el][] and requires Emacs).

Parenlab is a dirty cross compiler which forces an s-expression front
end onto Matlab, a bit like parenscript for Javascript.

It is also a case study in using shadchen, my pattern matching
library, to write nice code.

More to come.  

More
====

Parenlab converts this:

    (addpath "~/elisp/utils/parenlab/")
    (setq file-name (lambda (file)
                              (with parts (tokenize file "/")
                                    ({} parts end))))
            (setq is-cross-model-file 
                  (lambda (f) (not (== (dsf f nil :train-perc -1) -1))))
            (setq files (ddirnames "ccModelsZeroForced" is-cross-model-file))
            (for i (: 1 (length files))
                 (setq file ({} files i))
                 (load file)
                 (plotCrossModel model)
                 (with file (strrep (file-name file)  ".txt" "")
                       (vprint ["crossModel" file])))

To:

    addpath('~/elisp/utils/parenlab/');
    fileName = @(file)funcall(@(parts)parts{end}, tokenize(file, '/'));
    isCrossModelFile = @(f)~(equalequal(dsf(f, [], 'trainPerc', -1), -1));
    files = ddirnames('ccModelsZeroForced', isCrossModelFile);
    for i = ((1):(length(files)))
      file = files{i};
      load(file);
      plotCrossModel(model);
      funcall(@(file)vprint([ 'crossModel' file ]), strrep(fileName(file), '.txt', ''));
    end
    ;

Parenlab uses Emacs Lisp to give matlab all the wonderful convenience
of s-expressions AND meta-programming while still being Matlab.  The
user can define new macros that operate at the level of the
s-expression representation of Matlab code.  Underneath, the generated
code has access to all the powerful analytic and plotting abilities of
Matlab (or Octave, if you are running it.)

[shadchen-el]:https://github.com/VincentToups/shadchen-el