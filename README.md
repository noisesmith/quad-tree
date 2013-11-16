    user> (require '[noisesmith.quad-tree.quad :as quad] :reload)
    nil
    user> (def q (quad/make-quad 0 0 1024 1024))
    #'user/q
    user> q
    #noisesmith.quad_tree.quad.quad{:ne nil, :se nil, :sw nil, :nw nil, :x 0, :x' 1024, :y 0, :y' 1024, :pr #{}}
    user> (quad/render q) ; test.svg now exists, it can be loaded in a browser or inkscape
    nil
    user> (quad/render (quad/insert q #(update-in % [:pr] assoc :color {:fg "#ffffff" :bg "#000000" :opacity 0.6}) (quad/make-quad 258 258 700 700))) ; colorizing "marked" paths so we can easily differentiate them in the svg
    nil
    user> 

Copyright © 2013 noisesmith@gmail.com justin glenn smith

Distributed under the Eclipse Public License, the same as Clojure.
