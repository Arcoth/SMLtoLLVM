#pragma once

#include "AbsynInterfaceBasic.hpp"

namespace SMLNJInterface {

namespace PersStamps {
  using persstamp = vector<word8>;
}

namespace Stamps {
  using pid = PersStamps::persstamp;
  struct stamp : LABELLED_VARIANT(
    (Special, string)
    (Global, struct {
      pid pid;
      int cnt;
    })
    (Fresh, int)
  );
}

}
