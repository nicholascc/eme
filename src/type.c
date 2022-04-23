#include "type.h"

#include <stdio.h>
#include "c-utils/darray.h"
#include "c-utils/hash.h"
#include "symbol_table.h"
#include "errors.h"
#include "ast.h"

GENERATE_DARRAY_CODE(Type, Type_Array);

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
      case TYPE_STRUCT:
      case TYPE_POLY_INSTANCE: {
        Compilation_Unit_Ptr_Array members;
        if(info->type == TYPE_STRUCT)
          members = info->data.struct_.members;
        else if(info->type == TYPE_POLY_INSTANCE)
          members = info->data.poly_instance.members;

        int final_alignment = 1;
        info->size = 0;
        for(int i = 0; i < members.length; i++) {
          Compilation_Unit *unit = members.data[i];
          assert(unit->type == UNIT_STRUCT_MEMBER);
          type_infer_compilation_unit(unit);
          s32 type_size = size_of_type(unit->data.struct_member.type);
          s32 alignment = alignment_of_size(type_size);
          if(alignment > final_alignment) final_alignment = alignment;
          if(info->size % alignment != 0) info->size += alignment - info->size % alignment;
          assert(info->size % alignment == 0);
          unit->data.struct_member.offset = info->size;

          info->size += type_size;
        }
        // finally, align to that final alignment for use in arrays.
        if(info->size % final_alignment != 0) info->size += final_alignment - info->size % final_alignment;
        break;
      }
      case TYPE_UNIQUE: {
        info->size = size_of_type(info->data.unique.type);
        break;
      }
      case TYPE_INT:
      case TYPE_UNKNOWN_INT:
      case TYPE_BOOL:
      case TYPE_UNKNOWN_STRING:
      case TYPE_UNKNOWN:
      case TYPE_POISON:
      default: assert(false);
    }

    info->sized = true;
    return info->size;
  }
}

s32 alignment_of_size(s32 size) {
  if(size < 0) assert(false);
  if(size == 1 || size == 0) return 1;
  if(size == 2) return 2;
  if(size <= 4) return 4;
  return 8;
}

void init_primitive_types() {
  Type_Info *infos = malloc(32*sizeof(Type_Info)); // just allocate more space than necessary
  int n = 0;
  infos[n] = (Type_Info) {TYPE_UNKNOWN, false, false, 0, {0}};
  UNKNOWN_TYPE = (Type) {0, &infos[n++]};
  infos[n] = (Type_Info) {TYPE_NOTHING, true, true, 0, {0}};
  NOTHING_TYPE = (Type) {0, &infos[n++]};
  infos[n] = (Type_Info) {TYPE_POISON, false, false, 0, {0}};
  POISON_TYPE = (Type) {0, &infos[n++]};
  infos[n] = (Type_Info) {TYPE_BOOL, true, true, 1, {0}};
  BOOL_TYPE = (Type) {0, &infos[n++]};
  infos[n] = (Type_Info) {TYPE_FTYPE, false, false, 0, {0}};
  FTYPE_TYPE = (Type) {0, &infos[n++]};

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

  infos[n] = (Type_Info) {TYPE_FLOAT, true, true, 4, {.float_={32}}};
  FLOAT32_TYPE = (Type) {0, &infos[n++]};
  infos[n] = (Type_Info) {TYPE_FLOAT, true, true, 8, {.float_={64}}};
  FLOAT64_TYPE = (Type) {0, &infos[n++]};
}

Type float_type_with(u8 width) {
  switch(width) {
    case 32: return FLOAT32_TYPE;
    case 64: return FLOAT64_TYPE;
    default: assert(false);
  }
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
  info->sized = false;
  info->sizing_seen = false;
  info->data.unknown_int = value;
  return (Type){0, info};
}

Type make_unique_type(Type original, symbol name) {
  Type_Info *info = malloc(sizeof(Type_Info));
  info->type = TYPE_UNIQUE;
  info->sized = false;
  info->sizing_seen = false;
  info->data.unique.type = original;
  info->data.unique.name = name;
  return (Type){0, info};
}

bool type_equals(Type a, Type b) {
  return a.info == b.info && a.reference_count == b.reference_count;
}

void print_type_info(Type_Info t) {
  switch(t.type) {
    case TYPE_UNKNOWN: printf("unknown"); break;
    case TYPE_NOTHING: printf("nothing"); break;
    case TYPE_POISON: printf("poison"); break;
    case TYPE_INT: {
      if(t.data.integer.is_signed) printf("s%i", t.data.integer.width);
      else printf("u%i", t.data.integer.width);
      break;
    }
    case TYPE_FLOAT: {
      printf("f%i", t.data.float_.width);
      break;
    }
    case TYPE_BOOL: printf("bool"); break;
    case TYPE_UNKNOWN_INT: printf("literal integer (%lli)", t.data.unknown_int); break;
    case TYPE_FTYPE: printf("type"); break;
    case TYPE_STRUCT: print_symbol(t.data.struct_.definition->symbol); break;
    case TYPE_POLY_INSTANCE: {
      print_symbol(t.data.poly_instance.definition->symbol);
      Scope *s = t.data.poly_instance.scope;
      printf("(");
      assert(s->type == TYPE_SCOPE);
      for(int i = 0; i < s->entries.length; i++) {
        if(i > 0) printf(", ");
        Scope_Entry e = s->entries.data[i];
        print_type(e.data.type.value);
      }
      printf(")");
      break;
    }
    case TYPE_UNIQUE: print_symbol(t.data.unique.name); break;
    default:
      printf("(unprintable type)"); break;
  }
}

void print_type(Type t) {
  for(int i = 0; i < t.reference_count; i++) printf("^");
  print_type_info(*t.info);
}

u64 hash_type_info_ptr(Type_Info *t) {
  return hash_u64((u64)t);
}

u64 hash_type(Type t) {
  return hash_combine(hash_u64(t.reference_count), hash_type_info_ptr(t.info));
}
