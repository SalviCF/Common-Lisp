(defvar *family*
      '((colin nil nil)
	(deirdre nil nil)
	(arthur nil nil)
	(kate nil nil)
	(frank nil nil)
	(linda nil nil)
	(suzanne colin deirdre)
	(bruce arthur kate)
	(charles arthur kate)
	(david arthur kate)
	(ellen arthur kate)
	(george frank linda)
	(hillary frank linda)
	(andre nil nil)
	(tamara bruce suzanne)
	(vincent bruce suzanne)
	(wanda nil nil)
	(ivan george ellen)
	(julie george ellen)
	(marie george ellen)
	(nigel andre hillary)
	(frederick nil tamara)
	(zelda vincent wanda)
	(joshua ivan wanda)
	(quentin nil nil)
	(robert quentin julie)
	(olivia nigel marie)
	(peter nigel marie)
	(erica nil nil)
	(yvette robert zelda)
	(diane peter erica)))

(defun father (person)
  "Returns a person's father."
  (second (assoc person *family*)))

;; testing
(father 'colin)
(father 'suzanne)

(defun mother (person)
  "Returns a person's mother."
  (third (assoc person *family*)))

;; testing
(mother 'colin)
(mother 'suzanne)

(defun siblings (person)
  "Returns a list of a person's siblings, including genetics half siblings."
  (mapcar #'(lambda (element) (when (or (equal (father person) (second element))
					(equal (mother person) (third element)))))
	  *family*))

;; testing
(siblings 'bruce)
