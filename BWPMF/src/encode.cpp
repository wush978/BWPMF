#include "stdafx.h"

using namespace Rcpp;

Dictionary cookie_dict, hostname_dict;

//[[Rcpp::export]]
SEXP serialize_cookie(SEXP Rpath = R_NilValue) {
  if (Rpath == R_NilValue) {
    return rcpp_serialize(cookie_dict, true, true);
  } else {
    return serialize(Rpath, cookie_dict);
  }
}

//'@export
//[[Rcpp::export("deserialize_cookie.raw")]]
void deserialize_cookie_raw(RawVector src) {
  rcpp_deserialize(cookie_dict, src, true, true);
}

//'@export
//[[Rcpp::export("deserialize_cookie.character")]]
void deserialize_cookie_path(const std::string& path) {
  deserialize(path, cookie_dict);
}

//[[Rcpp::export]]
SEXP serialize_hostname(SEXP Rpath = R_NilValue) {
  if (Rpath == R_NilValue) {
    return rcpp_serialize(hostname_dict, true, true);
  } else {
    return serialize(Rpath, hostname_dict);
  }
}

//'@export
//[[Rcpp::export("deserialize_hostname.raw")]]
void deserialize_hostname_raw(RawVector src) {
  rcpp_deserialize(hostname_dict, src, true, true);
}

//'@export
//[[Rcpp::export("deserialize_hostname.character")]]
void deserialize_hostname_path(const std::string& path) {
  deserialize(path, hostname_dict);
}

void encode(const std::string& key, Dictionary& dict) {
  if (dict.find(key) == dict.end()) {
    size_t value = dict.size();
    dict.insert(std::make_pair(key, value));
  }
}

void encode_cookie(const std::string& cookie) {
  encode(cookie, cookie_dict);
}

void encode_hostname(const std::string& hostname) {
  encode(hostname, hostname_dict);
}

size_t query(const std::string& key, const Dictionary& dict) {
  auto itor = dict.find(key);
  if (itor == dict.end()) return NA_INTEGER;
  return itor->second;
}

SEXP query_vector(const CharacterVector& src, const Dictionary& dict) {
  NumericVector retval(src.size());
  for(int i = 0;i < src.size();i++) {
    retval[i] = query(CHAR(src[i]), dict);
  }
  return retval;
}

//[[Rcpp::export]]
SEXP query_cookie(CharacterVector cookie) {
  return query_vector(cookie, cookie_dict);
}

//[[Rcpp::export]]
SEXP query_hostname(CharacterVector hostname) {
  return query_vector(hostname, hostname_dict);
}

//[[Rcpp::export]]
SEXP count_cookie() {
  return wrap(cookie_dict.size());
}

//[[Rcpp::export]]
SEXP count_hostname() {
  return wrap(hostname_dict.size());
}

//[[Rcpp::export]]
void clean_cookie() {
  cookie_dict.clear();
  cookie_dict.reserve(0);
}

//[[Rcpp::export]]
void clean_hostname() {
  hostname_dict.clear();
  hostname_dict.reserve(0);
}

const std::string& first_comp(const std::string& src, const char sep3) {
  static std::string buf;
  auto delim = src.find(sep3);
  // Rprintf("%zd\n", delim);
  if (delim == std::string::npos) {
    buf.resize(0);
  } else {
    buf.assign(src, 0, delim);
  }
  return buf;
}

const std::pair<std::string, int>& two_comp(const std::string& src, const char sep3) {
  static std::pair<std::string, int> retval;
  static std::string buf;
  auto delim = src.find(sep3);
  buf.assign(src, delim + 1, src.size() - delim - 1);
  if (delim == std::string::npos) {
    retval.first.resize(0);
  } else {
    retval.first.assign(src, 0, delim);
    retval.second = std::atoi(buf.c_str());
  }
  return retval;
}

//'@title Encode the Source File for Calculating
//'@param x string. The path to the source file.
//'@param user_visit_lower_bound integer. If the user visit is less than 
//'   \code{user_visit_lower_bound}, then he/she will be filtered out.
//'@param progress numeric. The number of lines of the source file. If 
//'   the progress is set non-zero, then a progress bar will be displayed.
//'@param sep1 single character. The delimiter between the user and the rest.
//'@param sep2 single character. The delimiter between the pairs of item and the count.
//'@param sep3 single character. The delimiter between the item and count.
//'@export
//[[Rcpp::export("encode.character")]]
void encode_character(const std::string& x, size_t user_visit_lower_bound = 0, double progress = 0,
            const std::string& sep1 = "\1", const std::string& sep2 = "\2", 
            const std::string& sep3 = "\3") {
  if (sep1.size() != 1 | sep2.size() != 1 | sep3.size() != 1) {
    throw std::invalid_argument("The nchar of sep1, sep2, and sep3 should be 1");
  }
  std::ifstream input(x.c_str());
  static std::vector<std::string> buf1, buf2;
  std::shared_ptr<boost::progress_display> pb(NULL);
  if (progress > 0) pb.reset(new boost::progress_display(progress));
  for(std::string str ; std::getline(input, str);) {
    if (progress > 0) pb->operator++();
    boost::split(buf1, str, boost::is_any_of(sep1));
    if (buf1.size() < 2) throw std::logic_error("invalid data");
    std::string& cookie(buf1[0]);
    boost::split(buf2, buf1[1], boost::is_any_of(sep2));
    if (buf2.size() < 1) throw std::logic_error("invalid user data");
    if (buf2.size() < user_visit_lower_bound) continue;
    encode_cookie(cookie);
    for(const std::string& s : buf2) {
      const std::string& s2(first_comp(s, sep3[0]));
      if (s2.size() > 0) {
        encode_hostname(s2);
      }
    }
  }
}

bool check_na(CharacterVector::Proxy element) {
  SEXP p = wrap(element);
  return p == NA_STRING;
}

//'@export
//[[Rcpp::export("encode.data.frame")]]
void encode_data_frame(
  DataFrame x,
  int user_index = 1,
  int item_index = 2,
  bool progress = true) {
  if (x.size() < user_index | x.size() < item_index) {
    throw std::invalid_argument("The index is out of range");
  }
  CharacterVector
    user(x[user_index - 1]),
    item(x[item_index - 1]);
  std::shared_ptr<boost::progress_display> pb(NULL);
  if (progress) pb.reset(new boost::progress_display(x.nrows()));
  for(size_t i = 0;i < x.nrows();i++) {
    if (progress) pb->operator++();
    if (check_na(user[i]) | check_na(item[i])) continue;
    encode_cookie(as<std::string>(user[i]));
    encode_hostname(as<std::string>(item[i]));
  }
}

//'@export
//[[Rcpp::export("encode_data.character")]]
SEXP encode_data_character(
    const std::string& x, 
    double progress = 0, 
    const std::string& sep1 = "\1", 
    const std::string& sep2 = "\2",
    const std::string& sep3 = "\3") {
  if (sep1.size() != 1 | sep2.size() != 1 | sep3.size() != 1) {
    throw std::invalid_argument("The nchar of sep1, sep2, and sep3 should be 1");
  }
  static std::vector<std::string> buf1, buf2;
  std::shared_ptr<boost::progress_display> pb(NULL);
  std::vector<std::vector<ItemCount> > history_buffer(cookie_dict.size(), std::vector<ItemCount>());
  {
    std::ifstream input(x.c_str());
    if (progress > 0) pb.reset(new boost::progress_display(progress));
    for(std::string str ; std::getline(input, str);) {
      if (progress > 0) pb->operator++();
      boost::split(buf1, str, boost::is_any_of(sep1));
      if (buf1.size() < 2) throw std::logic_error("invalid data");
      std::string& cookie(buf1[0]);
      auto itor_cookie = cookie_dict.find(cookie);
      if (itor_cookie == cookie_dict.end()) continue;
      std::vector<ItemCount>& UserData(history_buffer[itor_cookie->second]);
      if (UserData.size() > 0) throw std::logic_error("Duplicated cookie");
      boost::split(buf2, buf1[1], boost::is_any_of(sep2));
      if (buf2.size() < 1) throw std::logic_error("invalid user data");
      for(const std::string& s : buf2) {
        const std::pair<std::string, int>& comp(two_comp(s, sep3[0]));
        if (comp.first.size() > 0) {
          auto itor_hostname = hostname_dict.find(comp.first);
          if (itor_hostname == hostname_dict.end()) throw std::logic_error("Unknown hostname");
          UserData.push_back(ItemCount(itor_hostname->second, comp.second));
        }
      }
    }
  }
  
  XPtr<History> retval(new History(history_buffer, hostname_dict.size()));
  retval.attr("class") = "history";
  return retval;
}

//'@export
//[[Rcpp::export("encode_data.data.frame")]]
SEXP encode_data_data_frame(
  DataFrame x,
  int user_index = 1,
  int item_index = 2,
  bool progress = true) {
  if (x.size() < user_index | x.size() < item_index) {
    throw std::invalid_argument("The index is out of range");
  }
  CharacterVector
    user(x[user_index - 1]),
    item(x[item_index - 1]);
  std::shared_ptr<boost::progress_display> pb(NULL);
  if (progress) pb.reset(new boost::progress_display(x.nrows()));
  std::vector<std::vector<ItemCount> > history_buffer(cookie_dict.size(), std::vector<ItemCount>());
  for(size_t i = 0;i < x.nrows();i++) {
    if (progress) pb->operator++();
    if (check_na(user[i]) | check_na(item[i])) continue;
    auto itor_cookie = cookie_dict.find(as<std::string>(user[i]));
    if (itor_cookie == cookie_dict.end()) continue;
    auto itor_hostname = hostname_dict.find(as<std::string>(item[i]));
    if (itor_hostname == hostname_dict.end()) continue;
    std::vector<ItemCount>& UserData(history_buffer[itor_cookie->second]);
    bool is_append = true;
    for(size_t j = 0;j < UserData.size();j++) {
      if (UserData[j].item != itor_hostname->second) continue;
      is_append = false;
      UserData[j].count += 1;
    }
    if (is_append) {
      UserData.push_back(ItemCount(itor_hostname->second, 1));
    }
  }
  XPtr<History> retval(new History(history_buffer, hostname_dict.size()));
  retval.attr("class") = "history";
  return retval;
}

//'@export
//[[Rcpp::export]]
SEXP serialize_history(SEXP Rhistory, SEXP Rpath = R_NilValue) {
  XPtr<History> phistory(Rhistory);
  History& history(*phistory);
  if (Rpath == R_NilValue) {
    return rcpp_serialize(history, true, true);
  } else {
    return serialize(Rpath, history);
  }
}

//'@export
//[[Rcpp::export("deserialize_history.raw")]]
SEXP deserialize_history_raw(RawVector src) {
  XPtr<History> retval(new History());
  rcpp_deserialize(*retval, src, true, true);
  return retval;
}

//'@export
//[[Rcpp::export("deserialize_history.character")]]
SEXP deserialize_history_path(const std::string& path) {
  XPtr<History> retval(new History());
  deserialize(path, *retval);
  return retval;
}

//'@export
//[[Rcpp::export("print.history")]]
void print_history(SEXP Rhistory) {
  XPtr<History> phistory(Rhistory);
  History& history(*phistory);
  for(size_t user = 0;user < history.user_size;user++) {
    Rprintf("user(%zu): ", user);
    history.data(user, [](const ItemCount& ic) {
      Rprintf("(item: %zu, count:%d) ", ic.item, ic.count);
    });
    Rprintf("\n");
  }
}

//'@export
//[[Rcpp::export]]
NumericVector check_history(SEXP Rhistory) {
  XPtr<History> phistory(Rhistory);
  History& history(*phistory);
  size_t error = 0, count = 0;
#pragma omp parallel for reduction( + : error, count )
  for(size_t user = 0;user < history.user_size;user++) {
    int local_error = 0, local_count = 0;
    std::set<size_t> check_tmp;
    history.data(user, [&check_tmp, &local_error, &local_count](const ItemCount& ic) {
      if (check_tmp.find(ic.item) != check_tmp.end()) local_error += 1;
      check_tmp.insert(ic.item);
      local_count += ic.count;
    });
    error += local_error;
    count += local_count;
  }
  if (error > 0) throw std::logic_error("Duplicated hostname in an user data");
  return wrap(count);
}

//'@export
//[[Rcpp::export]]
size_t count_non_zero_of_history(SEXP Rhistory) {
  XPtr<History> phistory(Rhistory);
  History& history(*phistory);
  size_t non_zero_count = 0;
  return history.data.get_total_size();
}

//'@export
//[[Rcpp::export]]
size_t count_cookie_history(SEXP Rhistory) {
  XPtr<History> phistory(Rhistory);
  History& history(*phistory);
  return history.user_size;
}

//'@export
//[[Rcpp::export]]
size_t count_hostname_history(SEXP Rhistory) {
  XPtr<History> phistory(Rhistory);
  History& history(*phistory);
  return history.item_size;
}

//'@export
//[[Rcpp::export]]
SEXP extract_history(SEXP Rhistory, NumericVector id) {
  XPtr<History> phistory(Rhistory);
  History& history(*phistory);
  std::sort(id.begin(), id.end());
  // calculate group of id
  std::vector<size_t> id_group(id.size(), 0);
  const size_t *start = history.data.get_index(), *end = history.data.get_index() + history.data.get_index_size();
#pragma omp parallel for
  for(size_t i = 0;i < id.size();i++) {
    auto j = std::upper_bound(start, end, id[i] - 1);
    auto group_idx = j - history.data.get_index();
    id_group[i] = group_idx;
  }
  // pick out these samples
  std::vector< std::vector<ItemCount> > new_history_buffer(history.user_size, std::vector<ItemCount>());
  {
    size_t i = id.size();
    while(i > 0) {
      i--;
#ifdef NOISY_DEBUG
      Rprintf("i: %zu\n", i);
      Rprintf("id[i]: %zu\n", (size_t) id[i]);
      Rprintf("id_group[i]: %zu\n", id_group[i]);
      Rprintf("cumulative_group_size[id_group[i] - 1]): %zu\n", history.data.size(id_group[i] - 1));
#endif
      size_t inner_group_id = (id_group[i] > 0 ? id[i] - start[id_group[i] - 1] : id[i]);
#ifdef NOISY_DEBUG
      Rprintf("inner_group_id: %zu\n", inner_group_id);
#endif
      if (inner_group_id > history.data.size(id_group[i] - 1)) throw std::logic_error("Invalid size ( > size )");
      if (inner_group_id == 0) throw std::logic_error("Invalid size ( == 0 )");
      ItemCount* target = history.data(id_group[i] - 1) + (inner_group_id - 1);
      new_history_buffer[id_group[i] - 1].push_back(*target);
      target->count = -1;
    }
  }
  history.data.clean([](const ItemCount& ic) {
    return ic.count > 0;
  });
  return XPtr<History>(new History(new_history_buffer, history.item_size));
}