(ns life.events
    (:require [re-frame.core :as re-frame]
              [life.db :as db]
              [life.rules :as rules]))

(re-frame/reg-event-db
 :initialize-db
 (fn  [_ _]
   db/default-db))

(re-frame/reg-event-db
 :tick
 (fn [db _]
   (update db :cells rules/step)))
