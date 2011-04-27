(ns guiftw.swing
  "Functions for Happy Swing User."
  (:require (guiftw [tree :as tree]
		    [props :as props])))


(defn swing-create
  "Function that instantiates object in Swing-specific manner. Calls
  ctor using optionally :*cons from style as parameters to create
  object. Then, if :*adder from parent style is not nil, calls it with
  (parent, parent-style, child, child-style.) Otherwise uses default
  adder, which calls parent.add(object) or parent.add(object,
  layout_constraints) if :*lay is present in style. Parent can be nil
  and then no adding happens. Returns created object."
  [ctor parent parent-style child-style]
  (let [specials (-> child-style props/get-value :specials)
	obj (apply ctor (:*cons specials))
	default-adder (fn [parent parent-style child child-style]
			(let [layout-data (-> child-style props/get-value :specials :*lay)]
			  (if layout-data (.add parent child layout-data)
			      (.add parent child))))
	adder (or (-> parent-style
		      props/get-value
		      :specials
		      :*adder)
		  default-adder)]
    (if parent
      (adder parent parent-style obj child-style))
    obj))

(defmacro swing
  "Parses GUI tree (struct) and return a function that creates GUI
  described by struct. For syntax of struct look into guiftw.tree
  doc.

  Uses *lay extra property to specify layout constraints (used when
  adding object to container)."
  [struct]
  `(tree/parse-gui swing-create ~struct))


(defn set-laf
  "Set look-and-feel by name. Throws exception if can't find laf."
  [laf]
  (->> (javax.swing.UIManager/getInstalledLookAndFeels)
       (filter #(-> % .getName (= laf)))
       first
       .getClassName
       javax.swing.UIManager/setLookAndFeel))

(defn lafs
  "Get available look-and-feel names."
  []
  (map #(.getName %)
       (javax.swing.UIManager/getInstalledLookAndFeels)))
