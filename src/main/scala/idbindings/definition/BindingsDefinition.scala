package com.github.kory33.s2mctest.protocolconversion
package idbindings.definition

case class BindingEntry(numberLiteral: String, className: String)
case class WithState(stateName: String,
                     serverBoundEntry: List[BindingEntry],
                     clientBoundEntry: List[BindingEntry])
case class VersionedBindings(versionName: String, states: List[WithState])
