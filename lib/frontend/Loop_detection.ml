(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core

type t = URI.Set.t
let empty = URI.Set.empty
let add_seen_uri = URI.Set.add
let add_seen_uri_opt uri_opt =
  match uri_opt with Some uri -> add_seen_uri uri | None -> Fun.id
let have_seen_uri = URI.Set.mem
let have_seen_uri_opt uri_opt =
  match uri_opt with Some uri -> have_seen_uri uri | None -> fun _ -> false
