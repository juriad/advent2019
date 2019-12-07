(defun readinput (filename)
	(with-open-file (in filename)
		(loop for line = (read-line in nil)
			while line 
			collect (let ((sep (search ")" line)))
				(cons (subseq line 0 sep) (subseq line (+ 1 sep)))
			)
		)
	)
)

(defun children (pairs root)
	(map
		'list
		#'cdr
		(remove-if-not 
			#'(lambda (pair) (equal (car pair) root))
			pairs
		)
	)
)

(defun dfs (pairs root)
	(let* (
		(cs (children pairs root))
		(ds (map 'list #'(lambda (c) (dfs pairs c)) cs))
	)
		(cons root ds)
	)
)


(defun sum-depths (tree depth)
	(if 
		(cdr tree) 
		(+ depth (reduce 
			#'+ 
			(map
				'list
				#'(lambda (sub) (sum-depths sub (+ 1 depth)))
				(cdr tree)
			)
		))
		depth
	)
)

(defun find-node (tree node path)
	(when
		tree
		(if
			(equal (car tree) node)
			path
			(let (
				(subs (map
					'list
					#'(lambda (sub) (find-node sub node (cons (car tree) path)))
					(cdr tree)
				))
			)
				(find-if
					'identity
					subs
				)
			)	
		)
	)
)

(defun diff (tree n1 n2)
	(let (
		(n1p (find-node tree n1 ()))
		(n2p (find-node tree n2 ()))
	)
		(progn
			(length (set-exclusive-or n1p n2p))
		)
	)
)

(let* (
	(pairs (readinput (car *args*)))
	(tree (dfs pairs "COM"))
)
	(print (sum-depths tree 0))
	(print (diff tree "YOU" "SAN"))
)

