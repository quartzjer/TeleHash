package org.eclipse.emf.json;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.codehaus.jackson.JsonFactory;
import org.codehaus.jackson.JsonParseException;
import org.codehaus.jackson.JsonParser;
import org.codehaus.jackson.JsonToken;
import org.codehaus.jackson.map.MappingJsonFactory;
import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EDataType;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.EcorePackage;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.emf.json.model.JSObject;

public class JsonUtil {

	static public EObject fromJson(String jsonTxt, EClass eClass) {
		JsonFactory f = new MappingJsonFactory();
		try {
			JsonParser jp = f.createJsonParser(jsonTxt.getBytes());
			return fromJson(jp, eClass);
		}
		catch (IOException e) {
			throw new RuntimeException(e);
		}
	}
	
	static public EObject fromJson(JsonParser jp, EClass eClass) throws JsonParseException, IOException {
		EObject result = eClass.getEPackage().getEFactoryInstance().create(eClass);
		Map<String, EStructuralFeature> fieldFeatureMap = getFieldFeatureMap(eClass);
		
		jp.nextToken(); // should == START_OBJECT?
		while (jp.nextToken() != JsonToken.END_OBJECT) {
			
			String fieldName = jp.getCurrentName();
			EStructuralFeature feature = fieldFeatureMap.get(fieldName);
			JsonToken nextToken = jp.nextToken();
			
			if (feature instanceof EAttribute) {
				
				EDataType dataType = (EDataType) feature.getEType();
				if (feature.isMany() && nextToken == JsonToken.START_ARRAY) {
					EList values = new BasicEList();
					while (jp.nextToken() != JsonToken.END_ARRAY) {
						values.add(parseValueForDataType(jp, dataType));
					}
					result.eSet(feature, values);
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
						values.add(fromJson(jp, eRef.getEReferenceType()));
					}
					result.eSet(feature, values);
				}
				else {
					result.eSet(feature, fromJson(jp, eRef.getEReferenceType()));
				}
			}
			else if (result instanceof JSObject) {
				JSObject jsObj = (JSObject) result;
				jsObj.getContents().put(fieldName, parseValueForUnmapped(nextToken, jp));
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

	private static Map<String, EStructuralFeature> getFieldFeatureMap(EClass eClass) {
		Map<String, EStructuralFeature> map = new HashMap<String, EStructuralFeature>();
		for (EStructuralFeature feature : eClass.getEAllStructuralFeatures()) {
			String keyType = EcoreUtil.getAnnotation(feature, "JsonMetadata", "keyType");
			StringBuilder keyBuilder = new StringBuilder();
			if (keyType != null) {
				if (keyType.equals("header")) {
					keyBuilder.append("_");
				}
				else if (keyType.equals("signal")) {
					keyBuilder.append("+");
				}
				else if (keyType.equals("command")) {
					keyBuilder.append(".");
				}
			}
			
			String key = EcoreUtil.getAnnotation(feature, "JsonMetadata", "key");
			keyBuilder.append(key == null ? feature.getName() : key);
			map.put(keyBuilder.toString(), feature);
		}
		return map;
	}

	static public String toJson(EObject eObj) {
		throw new UnsupportedOperationException();
	}
	
}
