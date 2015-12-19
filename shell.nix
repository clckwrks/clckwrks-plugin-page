{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, acid-state, aeson, attoparsec, base, clckwrks
      , containers, directory, filepath, happstack-hsp, happstack-server
      , hsp, hsx2hs, ixset, mtl, old-locale, random, reform
      , reform-happstack, reform-hsp, safecopy, stdenv, tagsoup
      , template-haskell, text, time, time-locale-compat, uuid
      , web-plugins, web-routes, web-routes-happstack, web-routes-th
      }:
      mkDerivation {
        pname = "clckwrks-plugin-page";
        version = "0.4.1";
        src = ./.;
        libraryHaskellDepends = [
          acid-state aeson attoparsec base clckwrks containers directory
          filepath happstack-hsp happstack-server hsp hsx2hs ixset mtl
          old-locale random reform reform-happstack reform-hsp safecopy
          tagsoup template-haskell text time time-locale-compat uuid
          web-plugins web-routes web-routes-happstack web-routes-th
        ];
        homepage = "http://www.clckwrks.com/";
        description = "support for CMS/Blogging in clckwrks";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};
  buildTools = [ pkgs.haskellPackages.hsx2hs ];
  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
