; *********************************************
; *  341  Programming Languages               *
; *  Fall 2016                                *
; *  Author: Liu Liu                          *
; *          Ulrich Kremer                    *
; *          Furkan Tektas , clisp            *
; *********************************************

;; ENVIRONMENT
;; "c2i, "i2c",and "apply-list"
(load "include.cl")

;; test document
(load "document.cl")

;; test-dictionary
;; this is needed for spell checking
(load "test-dictionary.cl")

;; (load "dictionary.cl") ;;  real dictionary (45K words)


(setq alfabe '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
(setq sifrem '(- - - - - - - - - - - - - - - - - - - - - - - - - -))
(setq Myfreq '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
(setq mostFrequentLetter '(e t a o i n))


;; -----------------------------------------------------
;; HELPERS
;; *** PLACE YOUR HELPER FUNCTIONS BELOW ***

;Sozlukte ayni harf sayisindaki kelimeleri ariyorum.dogrumu fonksiyonu ile cakisan harf varmi diye bakiyorum.
;Varsa Cakisan harf sozlugumdeki kelime yi ilerletiyorum.yoksa Code-Breaker a yolluyorum.
(defun MyLoop (myList)
	(setq bulundu 0)
	(setf yedekSozluk (copy-list *dictionary*))
	(dolist (template yedekSozluk)
		(if(= bulundu 0)
			(if(= (length template) (length myList))
				(progn
					(print alfabe)
					(print sifrem)
					(print myList)
					(princ " Eşittir ")
					(princ template)

					
					(if (equal(Perfect myList template) nil)
						(setf yedekSozluk (cdr yedekSozluk))
						;return nil'se o kelimenin koydugu harfleri sifirliyor.Yoksa bulmustur.Hemen cikiyorum.
						(if (equal(Code-Breaker myList template) nil)
							(progn
								(AlphabetReset myList))
							(setq bulundu 1))))))))



;Harfin durumunu bosmu veya koyulmasi gereken ile ayni harf mi diye bakan degilse nil return eden fonksiyon.
;Eger herhangi bir sorun yoksa gerekli yere harfi koyuyor.
(defun ChangeCharacter (harf1 harf2)
	(setq myIndex (c2i harf1))
	(if (or (eq (nth myIndex sifrem) '-) (eq (nth myIndex sifrem) harf2))
		(progn
			(setf (nth myIndex sifrem) harf2))
		(progn
			(print harf1)
			(princ " harfi Dolu")
			(return-from ChangeCharacter) nil)))



;Recursive
;Parametre olarak gelen iki kelimenin harflerine tek tek bakiyorum.
;Eger hatali deger varsa nil return ediyorum.
(defun Perfect (document decoder)
  	(cond
		((null document))
		((symbolp(car document))
  			(progn	
  				;Eger hatali deger varsa nil return ediyorum.
				(if (equal (LookCharacter (car document) (car decoder)) nil)
            		(return-from Perfect) nil)
  				(Perfect (cdr document) (cdr decoder))))))



;Harfin durumunu bosmu veya onceki ile ayni harf mi diye bakan degilse nil return eden fonksiyon.
;ChangeCharacter fonksiyonundan farki alfabe de degisiklik yapmamasi.Sadece uygun mu diye bakiyorum.
(defun LookCharacter (harf1 harf2)
	(setq myIndex (c2i harf1))
	(if (or (eq (nth myIndex sifrem) '-) (eq (nth myIndex sifrem) harf2))
		(progn
			(print harf1)(princ "---> ")(princ harf2))
		(progn
			(print harf1)(princ " harfinin alfabedeki yeri dolu...")
			(return-from LookCharacter) nil)))



;Recursive
;Gelen kelimenin harflerinin karsiligini sifirlayan fonksiyon.
(defun AlphabetReset (document)
	(print document)
	(cond
		((null document))
		((symbolp(car document))
  			(progn	
				(CharacterReset (car document))
  				(AlphabetReset (cdr document))))))



;Gelen harfin degerini alfabemde sifirliyorum.
(defun CharacterReset (harf1)
	(setq myIndex (c2i harf1))
	(setf (nth myIndex sifrem) '-))	



;harfin paragrafta kac tane oldugunu bulan fonksiyon.Yani harfin kac tane oldugunu hesapliyorum. 
(defun CharacterFrequency (world)
	(dotimes (i (length world))
		(setf harfim (nth i world))
		(setf index1 (c2i harfim))
		(setf (nth index1 Myfreq) (+ 1 (nth index1 Myfreq)))))
	

	
;paragraflari kelimeye parcaliyan fonksiyon.Kelime(not list) olunca CharacterFrequency yi cagiriyorum.
(defun parse1 (paragraph)
	(cond
		((null paragraph)) 
		((symbolp (car paragraph))
	 		(CharacterFrequency paragraph))
	 	((listp (car paragraph))
			(parse1 (car paragraph))
			(parse1 (cdr paragraph)))))



;En cok bulunan harfin karsiligini mostFrequentLetter'da ki harflerle degistiren fonksiyon. 
;maxElement e myFreq de ki maximum elemani veriyorum. 
;maxElement sayisi 0 olunca fonksiyondan cikiyorum.
(defun start-Freq()
	(setq tempMyfreq (copy-seq Myfreq))
	(dotimes (i 6)
		(setf maxElement (apply 'max tempMyfreq))
		(setf pos(position maxElement tempMyfreq))
			(if(equal maxElement 0)
				(return-from start-Freq nil)
				(progn
					(setf (nth pos sifrem) (nth i mostFrequentLetter))
					(setf (nth pos tempMyfreq) 0)))))



;paragraflari kelimeye parcaliyan fonksiyon.Kelime(not list) olunca enCokGecenKelime yi cagiriyorum.
(defun parse2 (paragraph sayi)
	(cond
		((null paragraph)) 
		((symbolp (car paragraph))
	 		(enCokGecenKelime paragraph sayi))
	 	((listp (car paragraph))
			(parse2 (car paragraph) sayi)
			(parse2 (cdr paragraph) sayi))))



;sayi parametresi boyutundaki kelimeleri bir liste de topluyorum.
(defun enCokGecenKelime (world sayi)
	(if (eq (length world) sayi)
		(push world dizi3harfli)))



;Verilen kelimenin(eleman) listede kac defa oldugunu bulan fonksiyon
(defun Counter (elememan liste)
  	(cond
   		((null liste) 0)
   		((equal elememan (car liste))
   			(+ 1 (Counter elememan (cdr liste))))
   		(t 
   			(Counter elememan (cdr liste)))))



;frq boyutunda paragrafta'ta en cok bulunan kelimeyi bulan fonksiyon. 
;kelime 1 harfli o kelimeyi -->a
;kelime 2 harfli o kelimeyi -->is
;kelime 3 harfli o kelimeyi -->the yaptim.
(defun SmartStrategy(paragraph frq)
	(setq dizi3harfli '(0))
	(parse2 paragraph frq)
	(setf tmpVar '())

	;Baslangicta listemde 1 eleman oldugu icin eger listem hala 1 ise frq boyutunda
	;paragrafta elemanim yok diye fonksiyondan cikiyorum.
	(if (eq (length dizi3harfli) 1)
		(progn
			(format t "~%~D harfli kelime bulunamadi...~%"frq)
			(return-from SmartStrategy nil))
		(dotimes (i (length dizi3harfli))
			(push (Counter (nth i dizi3harfli) paragraph) tmpVar)))

	;push ile ekleme yaptigim icin ters cevirme islemi yaptim.
	(setf tmpVar(reverse tmpVar))
	(setf maxElement (apply 'max tmpVar))
	(setf pos(position maxElement tmpVar))

	;Parametre olarak gelen degiskenin degerine gore str'nin degeri degisiyor.
	(cond
		((equal frq 1)(setf str '(a)))
		((equal frq 2)(setf str '(i s)))			
		((equal frq 3)(setf str '(t h e))))

	;Ekrana bilgilendirme mesaji.
	(format t "~%En cok gecen ~D harfli kelime --->~S  ve ~S ile degistirilecek...~%"frq (nth pos dizi3harfli) str)
	
	;Paragrafta en cok gecen kelime ile o boyuttaki str degiskenini Code-Breaker fonksiyonuma gonderiyorum.
	(if (equal (Code-Breaker (nth pos dizi3harfli) str)nil)
		(progn
			(AlphabetReset (nth pos dizi3harfli)))
		;--------
		(setq bulundu 1))
)



;; -----------------------------------------------------
;; DECODE FUNCTIONS



;Recursive
;Listeyi parcaliyorum.Sembolse(not list) myLoop fonksiyonuma gonderiyorum.
(defun Gen-Decoder-A (paragraph)
	(cond
		((null paragraph))
		((symbolp (car paragraph))
	 		(MyLoop paragraph))
	 	((listp (car paragraph))
			(Gen-Decoder-A (car paragraph))
			(Gen-Decoder-A (cdr paragraph)))))



;Gelen paragrafda once en cok gecen harfleri bulup mostFrequentLetter'da ki sirada ki harflerle yerini degistiriyorum.
;Bu sonuca gore baslangic icin alfabe olusturuyorum.Daha sonra Gen-Decoder-A fonksiyonumu cagiriyorum.
(defun Gen-Decoder-B-0 (paragraph)
	(parse1 paragraph)
	(print paragraph)
	(print "*************************************************************")
	(start-Freq)
	(print alfabe)
	(print Myfreq)
	(print mostFrequentLetter)
	(print "*************************************************************")
	(Gen-Decoder-A paragraph)
	(print alfabe)
	(print sifrem))



;Gelen parametreyi SmartStrategy fonksiyonuna gonderiyorum.Bu fonksiyonun 2.parametresi 1 harften,2 harften,
;3 harften olusan kelimeleri bulmasi icin.
(defun Gen-Decoder-B-1 (paragraph)
	(print paragraph)
	(SmartStrategy paragraph 1)
	(SmartStrategy paragraph 2)
	(SmartStrategy paragraph 3)
	(print alfabe)
	(print sifrem))



;Recursive
;Parametre olarak gelen iki kelimenin harflerini tek tek esliyorum.
;Eger hatali deger varsa nil return ediyorum.
(defun Code-Breaker (document decoder)
  	(cond
		((null document))
		((symbolp(car document))
  			(progn	
  				;Eger hatali deger varsa nil return ediyorum.
				(if (equal (ChangeCharacter (car document) (car decoder)) nil)
            		(return-from Code-Breaker) nil)
  				(Code-Breaker (cdr document) (cdr decoder))))))



;; ----------------------------------------------------------------------------------------------------------
;; ----------------------------------------------------------------------------------------------------------
;; ----------------------------------------------------------------------------------------------------------
;Main Kismi

;Part1 icin gerekli kod
(print "PART1")
(Gen-Decoder-A *test-document*)


;--------------------------------------------ONEMLI---------------------------------------------------------
;Part2 1.Kisim icin gerekli kod 
(print "PART2 1.KISIM")
(Gen-Decoder-B-0 *test-document*)


;Part2 2.Kisim icin gerekli kod 
(print "PART2 2.KISIM")
(Gen-Decoder-B-1 *test-document*)
