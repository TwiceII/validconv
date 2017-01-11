;;;
;;; Библиотека для валидации и конвертации данных
;;; (обычно для приходящих данных с веба)
;;;
(ns validconv.core
  (:require [clojure.string :refer [blank? lower-case]]))


;; 1. Обычные случаи - строго по полям, каждому полю одна валидация, одно соотв. поле
;; 2. Нестандартные ситуации - нестандартные проверки для валидации, нестандартные конвертирования

;; самый нестандартный случай при валидации:
;; когда зависит от всего хм и плюс от предыдущих проверок

;; одна общая структура данных для валидации и конвертации,
;; но ф-ции сделать две отдельные: отдельно валидация, и отдельно конвертация

;; конвертация ничего не знает о валидации,
;; она просто пытается конвертировать один хм в другой
;; если этого не получается, то выбрасывает исключение

;; нестандартные конвертирования:
;; - объединить два поля в одно
;; - переименовывание
;; - удаление полей
;; - дефолтное значение


(def example-config
  {:fields [{:f-key :amount
             :f-name "Количество"
             :valids [:required? "Заполните количество"
                      ;:int? "Кол-во должно быть целым числом"
                      ]
             :convs {:to-type :->int :def-value 0}}

            {:f-key :name
             :f-name "Наименование"
             :valids [:required? "Введите наименование" :custom? "Еще ошибка"]
             :convs {:to-type :->str :to-name :doc_name}}]

   :pre-valids [:custom-pre "Не прошло пре-валидацию"]
   :post-convs (fn [c] (-> c
                           assoc :newfield 47))
   })


(defmulti check-valid
  "Мультиметод для валидации
  Возвращает либо строку с ошибкой, если возникла, либо nil, если все нормально"
  (fn [row v-key extra-params] (-> v-key)))


(defn field-error-when
  [extra-params pred]
  (let [{:keys [f-key f-name error-tmpl]} extra-params]
    (when pred
      (format error-tmpl (or f-name f-key)))))


(defmethod check-valid :required?
  [row v-key extra-params]
  (field-error-when extra-params
                    (blank? (get row (:f-key extra-params)))))


(defmethod check-valid :custom?
  [row v-key extra-params]
  (field-error-when extra-params
                    (not= (get row (:f-key extra-params))
                          "custom")))


(check-valid {:name "qw"}
             :required?
             {:f-key :name
              :f-name "Имя"
              :error-tmpl "Не заполнено поле \"%s\""
              :prev-errors {}})


(check-valid {:custf "custom2"}
             :custom?
             {:f-key :custf
              :f-name "Кастомное поле"
              :error-tmpl "Не заполнено %s"
              :prev-errors {}})

(defn check-field
  [row field-params prev-errors]
  (let [{:keys [f-key f-name valids]} field-params]
    (reduce (fn [all-errors [v-key error-tmpl]]
              (if-let [error-msg (check-valid row
                                              v-key
                                              ;; extra-params
                                              {:f-key (:f-key field-params)
                                               :f-name (:f-name field-params)
                                               :error-tmpl error-tmpl
                                               :prev-errors all-errors})]
                (assoc-in all-errors [f-key v-key] error-msg)
                ;; нет ошибок
                all-errors))
            prev-errors (partition 2 valids))))


(check-field {:name "" :amount "3432"}
             {:f-key :name
              :f-name "Наименование"
              :valids [:required? "Введите наименование" :custom? "Еще ошибка"]
              :convs {:to-type :->str :to-name :doc_name}}
             {})


(defn validate-fields
  [row row-params all-errors]
  (reduce (fn [errors f-params]
            (check-field row f-params errors))
          all-errors (:fields row-params)))

(defn validate
  [row row-params]
  (->> {}
       (validate-fields row row-params)))


(validate {:name "" :amount ""}
          example-config)

;; (defmethod check-valid :custom-name?
;;   [[v-key error-tmpl] row field-key field-name prev-errors]
;;   (when (not= (get row field-key) "custom")
;;     (format error-tmpl field-name)))



;; (check-validation [:custom-name? "Не то имя"]
;;                   {:name "custom"}
;;                   :name
;;                   "Имя"
;;                   {})

;; (defn check-field
;;   [row field-params prev-errors]
;;   (if-let [field-errors (reduce (fn [ferrors [v-key error-tmpl]]
;;                                   (if-let [error-str (check-validation [v-key error-tmpl]
;;                                                                        row
;;                                                                        f



(def test-row {:name "" :count "2"})
(def test-field-params {:name {:fname "Наименование"
                               :validations [:required? "ЗАполните поле %s"
                                             :custom-name? "Еще ошибка"]}
                        :count {:fname "Количество"
                                :validations [:required? "Введите число"]}})

;; (defn check-validations
;;   [prev-errors row f-key f-name validation-pairs]
;;   (reduce (fn [errors v-pair]
;;             (let [[v-key error-tmpl] v-pair]
;;               (if-let [error-str (check-validation prev-errors
;;                                                    row
;;                                                    f-key
;;                                                    f-name
;;                                                    v-pair)]
;;                 (update errors f-key (fn[err-m] (assoc err-m v-key error-str)))
;;                 errors)))
;;           {} (partition 2 validation-pairs)))

;; (defmethod validate-field :custom-name?
;;   [prev-errors row field-key field-name v-pair]
;;   (println "validate-field custom-name")
;;   (let [[v-key error-tmpl] v-pair]
;;     (when (not= (get row field-key) "custom")
;;       (format error-tmpl field-name))))

;; (check-validations {}
;;                    {:name "" :amount "2"}
;;                    :name
;;                    "Название"
;;                    [:required? "Заполните это поле %s"
;;                     :custom-name? "Еще ошибка %s"
;;                     ])


;; (defmethod validate-field :custom-1
;;   [prev-errors row field-key field-name v-key error-tmpl]
;;   (when (get-in prev-errors [:amount :required?])


;; (def test-row {:name "" :count "2"})
;; (def test-field-params {:name {:fname "Наименование"
;;                                :validations [:required? "ЗАполните поле %s"]}
;;                         :count {:fname "Количество"
;;                                 :validations [:required? "Введите число"]}})





;; (def errors
;;   {:general {:required? "Не заполнено поле"
;;              :custom-1 "Кастомная ошибка"}

;;    :docname [{:error-msg "Не заполнено поле"
;;               :error-key :required?}
;;              {:error-msg "Заполнено неправильно"
;;               :error-lvl :normal}]})

;; (def validations {:required? {:error-template "Не заполнено поле \"%s\""}
;;                   :int? {:error-template "Число \"%s\" в неправильном формате"}})



;; (def validations {:required? {:v-key :required?
;;                               :error-tmpl "Не заполнено поле \"%s\""
;;                               :v-fn #

;; (println (format "Не заполнено поле \"%s\"" "name"))
;; (println (format "Не заполнено поле " "name"))

;; (defn add-error
;;   [errors error-msg]
;;   (conj errors































