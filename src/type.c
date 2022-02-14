#include "type.h"

#include <stdio.h>
#include "c-utils/darray.h"
#include "symbol_table.h"
#include "errors.h"
#include "ast.h"

u32 size_of_type(Type t) {
  if(t.reference_count > 0) return 8;
  else {
    Type_Info *info = t.info;
    if(info->sized) return info->size;
    if(info->sizing_seen) {
      if(info->type == TYPE_STRUCT) {
        print_error_message("This struct includes itself as a member or member of a member, which is illegal.", info->data.struct_.definition->n.loc);
        exit(1);
      } else assert(false && "non-struct object has circular size dependency");
    }
    info->sizing_seen = true;

    switch(info->type) {
      case TYPE_STRUCT: {
        info->size = 0;
        for(int i = 0; i < info->data.struct_.members.length; i++) {
          Struct_Member *member = &info->data.struct_.members.data[i];
          Compilation_Unit *unit = member->unit;
          assert(unit->type == UNIT_STRUCT_MEMBER);
          infer_types_of_compilation_unit(unit);
          s32 type_size = size_of_type(unit->data.struct_member.type);
          s32 alignment = alignment_of_size(type_size);
          if(info->size % alignment != 0) info->size += alignment - info->size % alignment;
          assert(info->size % alignment == 0);
          member->offset = info->size;

          info->size += type_size;
        }
        break;
      }
      case TYPE_INT:
      case TYPE_UNKNOWN_INT:
      case TYPE_BOOL:
      case TYPE_UNKNOWN:
      case TYPE_NOTHING:
      case TYPE_POISON:
      default: assert(false);
    }

    info->sized = true;
    return info->size;
  }
}

s32 alignment_of_size(s32 size) {
  if(size < 1) assert(false);
  if(size == 1) return 1;
  if(size == 2) return 2;
  if(size <= 4) return 4;
  return 8;
}

void init_primitive_types() {
  Type_Info *infos = malloc(32*sizeof(Type_Info)); // just allocate more space than necessary
  int n = 0;
  infos[n] = (Type_Info) {TYPE_UNKNOWN, true, true, 0, {0}};
  UNKNOWN_TYPE = (Type) {0, &infos[n++]};
  infos[n] = (Type_Info) {TYPE_NOTHING, true, true, 0, {0}};
  NOTHING_TYPE = (Type) {0, &infos[n++]};
  infos[n] = (Type_Info) {TYPE_POISON, true, true, 0, {0}};
  POISON_TYPE = (Type) {0, &infos[n++]};
  infos[n] = (Type_Info) {TYPE_BOOL, true, true, 1, {0}};
  BOOL_TYPE = (Type) {0, &infos[n++]};

  for(int i = 0; i < 2; i++) {
    bool is_signed = i;
    for(int j = 0; j < 4; j++) {
      u8 width_bytes;
      switch(j) {
        case 0: width_bytes = 1; break;
        case 1: width_bytes = 2; break;
        case 2: width_bytes = 4; break;
        case 3: width_bytes = 8; break;
        default: assert(false);
      }
      infos[n] = (Type_Info) {TYPE_INT, true, true, width_bytes, {.integer={is_signed,width_bytes*8}}};
      INTEGER_TYPES[i][j] = (Type) {0, &infos[n++]};
    }
  }
  INT_TYPE = INTEGER_TYPES[1][3];
  UINT_TYPE = INTEGER_TYPES[0][3];
}

Type integer_type_with(bool is_signed, u8 width) {
  switch(width) {
    case 8: return INTEGER_TYPES[is_signed][0];
    case 16: return INTEGER_TYPES[is_signed][1];
    case 32: return INTEGER_TYPES[is_signed][2];
    case 64: return INTEGER_TYPES[is_signed][3];
    default: assert(false);
  }
}

Type allocate_unknown_int_type(int value) {
  Type_Info *info = malloc(sizeof(Type_Info));
  info->type = TYPE_UNKNOWN_INT;
  info->data.unknown_int = value;
  return (Type){0, info};
}

bool type_equals(Type a, Type b) {
  return a.info == b.info && a.reference_count == b.reference_count;
}

void print_type_info(Type_Info t) {
  switch(t.type) {
    case TYPE_UNKNOWN: printf("unknown"); break;
    case TYPE_NOTHING: printf("nothing"); break;
    case TYPE_INT: {
      if(t.data.integer.is_signed) printf("s%i", t.data.integer.width);
      else printf("u%i", t.data.integer.width);
      break;
    }
    case TYPE_BOOL: printf("bool"); break;
    case TYPE_UNKNOWN_INT: printf("literal integer (%lli)", t.data.unknown_int); break;
    case TYPE_STRUCT: print_symbol(t.data.struct_.definition->symbol); break;
    default:
      printf("(unprintable type)"); break;
  }
}

void print_type(Type t) {
  for(int i = 0; i < t.reference_count; i++) printf("*");
  print_type_info(*t.info);
}
