#include "glean/rts/sanity.h"
#include <gflags/gflags.h>

DEFINE_bool(sanity_checks, false, "perform sanity checkes (slow)");

namespace facebook {
namespace glean {
namespace rts {

bool Sanity::enabled = FLAGS_sanity_checks;

}
}
}
