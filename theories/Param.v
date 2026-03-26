From Ltac2 Require Import Ltac2.
Declare ML Module "coq-paramcoq.plugin".

Ltac2 @ external parametricity_base_type : constr -> constr
  := "coq-paramcoq.plugin" "parametricity_base_type".
Ltac2 @ external parametricity_base_term : constr -> constr
  := "coq-paramcoq.plugin" "parametricity_base_term".
Ltac2 @ external realizer_base_type : constr -> constr
  := "coq-paramcoq.plugin" "realizer_base_type".
Ltac2 @ external realizer_base_term : constr -> constr
  := "coq-paramcoq.plugin" "realizer_base_term".
