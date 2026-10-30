;; Copyright 2026 Marcos Magueta <maguetamarcos@gmail.com>
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 2.1 of the License, or (at your option) any later version.

;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

(defmodule fol
  (export (base 1) (project 2) (source 1)))

(defun base (path)
  "A plan that reads the relation or program at Path (a list of
binaries); RNT's FOL Base."
  (tuple 'base path))

(defun project (plan attributes)
  "A plan that keeps only Attributes of Plan; RNT's FOL Project."
  (tuple 'project plan attributes))

(defun source
  "Serialises a plan into the bencode RNT's fol evaluator parses, ready
for rnt:define or rnt:evaluate. Dict keys stay in sorted order."
  (((tuple 'base path)) (tagged #\b (bencode-list path)))
  (((tuple 'project plan attributes))
   (tagged #\p (list #\d
                     (bencode #"attributes") (bencode-list attributes)
                     (bencode #"term") (source plan)
                     #\e))))

(defun tagged (tag value)
  "Bencode extension RNT uses for constructors: t, a tag byte, a value."
  (list #\t tag value))

(defun bencode (bytes)
  "A bencode string: its length, a colon, the bytes."
  (list (integer_to_binary (byte_size bytes)) #\: bytes))

(defun bencode-list (items)
  "A bencode list of strings."
  (list #\l (lists:map (fun bencode 1) items) #\e))
