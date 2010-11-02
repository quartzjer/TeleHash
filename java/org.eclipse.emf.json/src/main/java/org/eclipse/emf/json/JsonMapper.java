package org.eclipse.emf.json;

import java.io.IOException;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.codehaus.jackson.JsonFactory;
import org.codehaus.jackson.JsonGenerationException;
import org.codehaus.jackson.JsonGenerator;
import org.codehaus.jackson.JsonParseException;
import org.codehaus.jackson.JsonParser;
import org.codehaus.jackson.JsonToken;
import org.codehaus.jackson.map.MappingJsonFactory;
import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.common.util.EMap;
import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EDataType;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.EcorePackage;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.emf.json.model.JsObject;
import org.eclipse.emf.json.model.JsonPackage;

public final class JsonMapper {

	private static final String JSON_KEY = "key";
	private static final String JSON_KEY_TYPE = "keyType";
	private static final String JSON_METADATA_ANN_URI = "JsonMetadata";
	private static final String JSON_KEYTYPE_COMMAND = "command";
	private static final String JSON_KEYTYPE_SIGNAL = "signal";
	private static final String JSON_KEYTYPE_HEADER = "header";

	public static EObject fromJson(String jsonTxt, EClass eClass) throws IOException {
		return new JsonMapper().from(jsonTxt, eClass);
	}
	
	private Map<EClass, Map<String, EStructuralFeature>> jsonMetadataCache =
		new HashMap<EClass, Map<String,EStructuralFeature>>();
	
	public EObject from(String jsonTxt, EClass eClass) throws IOException {
		JsonFactory f = new MappingJsonFactory();
		JsonParser jp = f.createJsonParser(jsonTxt.getBytes());
		return from(jp, eClass);
	}
	
	private EObject from(JsonParser jp, EClass eClass) throws JsonParseException, IOException {
		EObject result = eClass.getEPackage().getEFactoryInstance().create(eClass);
		Map<String, EStructuralFeature> fieldFeatureMap = getFieldFeatureMap(eClass);
		
		for (JsonToken nextToken = jp.nextToken(); nextToken != JsonToken.END_OBJECT; nextToken = jp.nextToken()) {
			
			// If this is start of object, skip to field
			if (nextToken == JsonToken.START_OBJECT) {
				nextToken = jp.nextToken(); // should be FIELD_NAME
				if (nextToken == JsonToken.END_OBJECT) {
					break;
				}
			}
			
			String fieldName = jp.getCurrentName();
			nextToken = jp.nextToken();
			EStructuralFeature feature = fieldFeatureMap.get(fieldName);
			
			if (feature instanceof EAttribute) {
				
				EDataType dataType = (EDataType) feature.getEType();
				if (feature.isMany()) {
					if (nextToken == JsonToken.START_ARRAY) {
						EList values = new BasicEList();
						while (jp.nextToken() != JsonToken.END_ARRAY) {
							values.add(parseValueForDataType(jp, dataType));
						}
						result.eSet(feature, values);
					}
				}
				else {
					result.eSet(feature, parseValueForDataType(jp, dataType));
				}
			}
			else if (feature instanceof EReference) {
				EReference eRef = (EReference) feature;
				if (!eRef.isContainment()) {
					continue; // Non-containment references are currently ignored
				}
				
				if (feature.isMany() && nextToken == JsonToken.START_ARRAY) {
					@SuppressWarnings("rawtypes")
					EList values = new BasicEList();
					while (jp.nextToken() != JsonToken.END_ARRAY) {
						values.add(from(jp, eRef.getEReferenceType()));
					}
					result.eSet(feature, values);
				}
				else {
					result.eSet(feature, from(jp, eRef.getEReferenceType()));
				}
			}
			else if (result instanceof JsObject) {
				JsObject jsObj = (JsObject) result;
				jsObj.getUnmatched().put(fieldName, parseValueForUnmapped(nextToken, jp));
			}
		}
		
		return result;
	}
	
	private static Object parseValueForUnmapped(JsonToken nextToken, JsonParser jp) 
	throws JsonParseException, IOException {
		if (nextToken == JsonToken.VALUE_FALSE || nextToken == JsonToken.VALUE_TRUE) {
			return jp.getBooleanValue();
		}
		else if (nextToken == JsonToken.VALUE_STRING) {
			return jp.getText();
		}
		else if (nextToken == JsonToken.VALUE_NUMBER_INT) {
			return jp.getIntValue();
		}
		else if (nextToken == JsonToken.VALUE_NUMBER_FLOAT) {
			return jp.getFloatValue();
		}
		else if (nextToken == JsonToken.START_OBJECT) {
			return jp.readValueAs(Map.class);
		}
		else if (nextToken == JsonToken.START_ARRAY) {
			List<Object> list = new ArrayList<Object>();
			for (JsonToken listToken = jp.nextToken(); listToken != JsonToken.END_ARRAY; 
					listToken = jp.nextToken()) {
				list.add(parseValueForUnmapped(listToken, jp));
			}
			return list;
		}
		else {
			return jp.getText();
		}
	}

	private static Object parseValueForDataType(JsonParser jp, EDataType dataType) throws JsonParseException, IOException {
		if (dataType.getEPackage() == EcorePackage.eINSTANCE) {
			switch (dataType.getClassifierID()) {
			case EcorePackage.EBOOLEAN:
				return jp.getBooleanValue();
			case EcorePackage.EBYTE:
				return jp.getByteValue();
			case EcorePackage.ESHORT:
				return jp.getShortValue();
			case EcorePackage.EINT:
				return jp.getIntValue();
			case EcorePackage.ELONG:
				return jp.getLongValue();
			case EcorePackage.EFLOAT:
				return jp.getFloatValue();
			case EcorePackage.EDOUBLE:
				return jp.getDoubleValue();
			case EcorePackage.ESTRING:
				return jp.getText();
			}
		}
		return dataType.getEPackage().getEFactoryInstance().createFromString(dataType, jp.getText());
	}

	private Map<String, EStructuralFeature> getFieldFeatureMap(EClass eClass) {
		Map<String, EStructuralFeature> map = jsonMetadataCache.get(eClass);
		if (map != null) {
			return map;
		}
		map = new HashMap<String, EStructuralFeature>();
		
		for (EStructuralFeature feature : eClass.getEAllStructuralFeatures()) {
			String key = getJsonFieldName(feature);
			map.put(key, feature);
		}
		
		jsonMetadataCache.put(eClass, map);
		return map;
	}

	static public String toJson(EObject eObj) throws IOException {
		return new JsonMapper().to(eObj);
	}
	
	public String to(EObject eObj) throws IOException {
		StringWriter result = new StringWriter();
		JsonGenerator jg = null;
		
		jg = new MappingJsonFactory().createJsonGenerator(result);
		to(eObj, jg);
		jg.flush();
		
		return result.toString();
	}

	private void to(EObject eObj, JsonGenerator jg) throws JsonGenerationException, IOException {
		EClass eClass = eObj.eClass();
		jg.writeStartObject();
		for (EStructuralFeature feature : eClass.getEStructuralFeatures()) {
			String jsonKey = getJsonFieldName(feature);
			if (feature.isUnsettable() && !eObj.eIsSet(feature)) {
				continue;
			}
			
			if (feature == JsonPackage.Literals.JS_OBJECT__UNMATCHED) {
				EMap<String, Object> unmatched = (EMap<String, Object>) eObj.eGet(feature);
				for (Map.Entry<String, Object> entry : unmatched.entrySet()) {
					jg.writeObjectField(entry.getKey(), entry.getValue());
				}
			}
			else if (feature instanceof EAttribute) {
				EDataType dataType = (EDataType) feature.getEType();
				if (feature.isMany()) {
					List<Object> childObjs = (List<Object>) eObj.eGet(feature);
					jg.writeFieldName(jsonKey);
					jg.writeStartArray();
					for (Object value : childObjs) {
						writeValueField(jg, null, dataType, value);
					}
					jg.writeEndArray();
				}
				else {
					Object value = eObj.eGet(feature);
					if (value != null) {
						writeValueField(jg, jsonKey, dataType, value);
					}
				}
			}
			else if (feature instanceof EReference) {
				if (feature.isMany()) {
					List<EObject> childObjs = (List<EObject>) eObj.eGet(feature);
					jg.writeStartArray();
					for (EObject childObj : childObjs) {
						to(childObj, jg);
					}
					jg.writeEndArray();
				}
				else {
					EObject childObj = (EObject) eObj.eGet(feature);
					if (childObj != null) {
						to(childObj, jg);
					}
				}
			}
		}
		jg.writeEndObject();
	}

	private void writeValueField(JsonGenerator jg, String fieldName,
			EDataType dataType, Object value) throws JsonGenerationException, IOException {
		if (fieldName != null) {
			jg.writeFieldName(fieldName);
		}
		if (dataType.getEPackage() == EcorePackage.eINSTANCE) {
			switch (dataType.getClassifierID()) {
			case EcorePackage.EBOOLEAN:
				jg.writeBoolean((Boolean)value);
				return;
			case EcorePackage.EBYTE:
				jg.writeNumber((Byte)value);
				return;
			case EcorePackage.ESHORT:
				jg.writeNumber((Short)value);
				return;
			case EcorePackage.EINT:
				jg.writeNumber((Integer)value);
				return;
			case EcorePackage.ELONG:
				jg.writeNumber((Long)value);
				return;
			case EcorePackage.EFLOAT:
				jg.writeNumber((Float)value);
				return;
			case EcorePackage.EDOUBLE:
				jg.writeNumber((Double)value);
				return;
			case EcorePackage.ESTRING:
				jg.writeString((String)value);
				return;
			}
		}
		jg.writeString(
				dataType.getEPackage().getEFactoryInstance().convertToString(dataType, value));
	}

	private static String getJsonFieldName(EStructuralFeature feature) {
		String key = EcoreUtil.getAnnotation(feature, JSON_METADATA_ANN_URI, JSON_KEY);
		if (key == null) {
			key = feature.getName();
		}
		
		StringBuilder keyBuilder = new StringBuilder();
		String keyType = EcoreUtil.getAnnotation(feature, JSON_METADATA_ANN_URI, JSON_KEY_TYPE);
		if (keyType != null) {
			if (keyType.equals(JSON_KEYTYPE_COMMAND)) {
				keyBuilder.append('.');
			}
			else if (keyType.equals(JSON_KEYTYPE_SIGNAL)) {
				keyBuilder.append('+');
			}
			else if (keyType.equals(JSON_KEYTYPE_HEADER)) {
				keyBuilder.append('_');
			}
		}
		keyBuilder.append(key);
		
		return keyBuilder.toString();
	}
	
}
