(ns mondokisa.core
  (:require [clj-http.client :as http]
            [clj-http.cookies :as cookies]
            )
  (:import (org.jsoup Jsoup))
  (:gen-class)
  )

(def +endomondo-profile-url+ "http://www.endomondo.com/profile/")
(def +kilometrikisa-url+ "https://www.kilometrikisa.fi/")

(def +kilometrikisa-cookies+ (cookies/cookie-store))
(def +kilometrikisa-opts+ {;; Use cookie store
                           :cookie-store +kilometrikisa-cookies+
                           ;; Java by default doesn't trust RapidSSL certs, use truststore
                           :trust-store "kilometrikisa.jks"
                           :trust-store-type "jks"
                           :trust-store-pass "changeit"})

(defn parse-date [d]
  "Parse Endomondo date without year to Kilometrikisa style ISO8601 with current year"

  ;; Date can also be like: "36 minutes ago"
  (if (re-matches #".* ago" d)
    ;; Assume today, for date like "XX minutes ago"
    (.format (java.text.SimpleDateFormat. "yyyy-MM-dd") (java.util.Date.))

    ;; Parse date like "April 26"
    (let [[_ month day] (first (re-seq #"(\w+) (\d+)" d))]
      (str (.get (java.util.Calendar/getInstance) java.util.Calendar/YEAR) "-"
           (case month
                 "January" "01"
                 "February" "02"
                 "March" "03"
                 "April" "04"
                 "May" "05"
                 "June" "06"
                 "July" "07"
                 "August" "08"
                 "September" "09"
                 "October" "10"
                 "November" "11"
                 "December" "12") "-"
           (format "%02d" (Integer/parseInt day))
           ))))

(defn parse-workout [news-div]
  "Parse a workout from Endomondo profile news item div."
  (let [sport (-> news-div (.select "b") .text)]
    {:sport sport
     :kilometers (-> (re-seq #"tracked ([\d\.]+)" (.text news-div))
                     first second Double/parseDouble)
     :date (-> news-div (.select "span.date") .text parse-date)
     }))


(defn fetch-workouts [id]
  (println "Fetch Endomondo workouts for profile " id)
  (let [parse-all #(map parse-workout %)]
    (-> (str +endomondo-profile-url+ id)
        http/get
        :body
        Jsoup/parse
        (.select "div.newsDetails")
        seq parse-all)))


(defn kilometrikisa-url [relative-url]
  (str +kilometrikisa-url+ relative-url))

(defn extract-login-token [{body :body}]
  "Extract CSRF middleware token from hidden field in login form."
  (-> body
      Jsoup/parse
      (.select "input[name=csrfmiddlewaretoken]")
      (.attr "value")))

(defn extract-contest-and-token [response]
  (let [js (-> response :body Jsoup/parse
               (.select "body div.page-title + script")
               seq first .html)]
    {:contest-id (second (first (re-seq #".*contestId\s*=\s*(\d+).*" js)))
     :csrf-token (second (first (re-seq #".*csrfToken\s*=\s*\"([^\"]+)\".*" js)))}))

(defn km-get [url opts]
  (http/get (kilometrikisa-url url)
            (merge +kilometrikisa-opts+ opts)))

(defn km-post [url opts]
  (http/post (kilometrikisa-url url)
             (merge +kilometrikisa-opts+ opts)))

(defn kilometrikisa-login [user password]
  (println "Logging in to Kilometrikisa as " user)
  (let [token (extract-login-token
               (km-get "accounts/login/" {}))]
    (km-post "accounts/login/"
             {:headers {"Referer" (kilometrikisa-url "accounts/login/")}
              :form-params {:csrfmiddlewaretoken token
                            :username user
                            :password password}})
    (let [cf (km-get "contest/log/"
                     {:headers {"Referer" (kilometrikisa-url "accounts/index/")}})]

          (extract-contest-and-token cf))))


(defn kilometrikisa-set-kilometers [session date kilometers]
  (println "Updating Kilometrikisa kilometers for date " date " to " kilometers)
  (km-post "contest/log-save/"
           {:form-params {:contest_id (:contest-id session)
                          :km_amount (str kilometers)
                          :km_date date
                          :csrfmiddlewaretoken (:csrf-token session)}
            :headers {"Referer" (kilometrikisa-url "contest/log/")}}))


(defn sync-endomondo-kilometrikisa [profile user password]
  "Sync sum of today's cycling to Kilometrikisa"
  (let [cycling (filter #(= (:sport %) "cycling") (fetch-workouts profile))
        cycling-by-date (group-by :date cycling)
        session (kilometrikisa-login user password)
        df (java.text.SimpleDateFormat. "yyyy-MM-dd")
        today (.format df (java.util.Date.))
        km (reduce + 0 (map :kilometers (get cycling-by-date today)))]
    (println "Sync today " today " cycling: " km " km")
    (kilometrikisa-set-kilometers session today km)
    ))

(defn -main [& args]
  (let [[profile user password] args]
    (if (and profile user password)
      (sync-endomondo-kilometrikisa profile user password)
      (println "Usage: give 3 parameters: endomondo profile id, kilometrikisa username and kilometrikisa password")
      )))

