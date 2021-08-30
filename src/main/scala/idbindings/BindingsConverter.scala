package com.github.kory33.s2mctest.protocolconversion
package idbindings

import idbindings.definition.{VersionedBindings, WithState}

object BindingsConverter {
  def convert(bindings: VersionedBindings): VersionedBindings = {
    bindings.copy(
      states = bindings.states.filter {
        case WithState(stateName, _, _) => !Set("handshake", "status").contains(stateName)
      }
    )
  }
}
