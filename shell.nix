with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, acid-state, aeson, attoparsec, base, clckwrks
             , containers, directory, filepath, happstack-hsp, happstack-server
             , hsp, hsx2hs, ixset, mtl, old-locale, random, reform
             , reform-happstack, reform-hsp, safecopy, stdenv, tagsoup
             , template-haskell, text, time, time-locale-compat, uuid
             , web-plugins, web-routes, web-routes-happstack, web-routes-th
             }:
             mkDerivation {
               pname = "clckwrks-plugin-page";
               version = "0.3.9";
               src = ./.;
               buildDepends = [
                 acid-state aeson attoparsec base clckwrks containers directory
                 filepath happstack-hsp happstack-server hsp hsx2hs ixset mtl
                 old-locale random reform reform-happstack reform-hsp safecopy
                 tagsoup template-haskell text time time-locale-compat uuid
                 web-plugins web-routes web-routes-happstack web-routes-th
               ];
               buildTools = [ hsx2hs ];
               homepage = "http://www.clckwrks.com/";
               description = "support for CMS/Blogging in clckwrks";
               license = stdenv.lib.licenses.bsd3;
             }) {};
in
  pkg.env
